(**************************************************************************)
(*                                                                        *)
(*                                 OCamlFDO                               *)
(*                                                                        *)
(*                     Greta Yorsh, Jane Street Europe                    *)
(*                                                                        *)
(*   Copyright 2019 Jane Street Group LLC                                 *)
(*                                                                        *)
(*   All rights reserved.  This file is distributed under the terms of    *)
(*   the GNU Lesser General Public License version 2.1, with the          *)
(*   special exception on linking described in the file LICENSE.          *)
(*                                                                        *)
(**************************************************************************)
open Core
open Core.Poly
open Ocamlcfg

let verbosity_level = 20

let verbose = ref true

let strict = ref false

let quiet () =
  verbose := false;
  Perf_profile.verbose := false;
  Aggregated_decoded_profile.verbose := false;
  Aggregated_perf_profile.verbose := false;
  Bolt_profile.verbose := false;
  Decoded_bolt_profile.verbose := false;
  Cfg_info.verbose := false;
  Clusters.verbose := false;
  Elf_locations.verbose := false;
  Ocaml_locations.verbose := false;
  Reorder.verbose := false;
  Report.verbose := false;
  Cfg_builder.verbose := false;
  Crcs.verbose := false;
  Wrapper.verbose := false;
  ()

let time f x =
  let open Time in
  let start = now () in
  let fx = f x in
  let stop = now () in
  printf "Execution time: %s\n" (Span.to_string (abs_diff stop start));
  fx

let print_linear msg f =
  if verbosity_level > 10 then
    if !verbose then (
      printf "%s processing %s\n" f.Linear.fun_name msg;
      Printlinear.fundecl Format.std_formatter f;
      Format.pp_print_flush Format.std_formatter () )

let make_fdo_filename file = file ^ "-fdo"

let load_locations binary_filename =
  let elf_locations =
    Elf_locations.create ~elf_executable:binary_filename
  in
  if !verbose then Elf_locations.print_dwarf elf_locations;
  elf_locations

let load_crcs locations tbl =
  let prefix = Crcs.symbol_prefix in
  let on = Crcs.symbol_sep in
  Elf_locations.iter_symbols locations ~f:(fun s ->
      match String.chop_prefix s ~prefix with
      | None -> ()
      | Some suffix -> (
          let name, hex = String.rsplit2_exn suffix ~on in
          let crc = Md5.of_hex_exn hex in
          if !verbose then (
            printf "crc_symbol=%s\n" s;
            printf "name=%s hex=%s\n" name hex );
          match Hashtbl.find tbl name with
          | None -> Hashtbl.set tbl ~key:name ~data:crc
          | Some old_crc ->
              (* The symbol can appear multiple times if it enters more than
                 one symbol tables, e.g., both static and dynamic. *)
              if not (Md5.equal old_crc crc) then
                failwithf "Duplicate crc for %s\nold:%s\nnew:%s\n" name
                  (Md5.to_hex old_crc) (Md5.to_hex crc) () ))

let save_linker_script_hot filename functions =
  if !verbose then printf "Writing linker script hot to %s\n" filename;
  Out_channel.with_file filename ~f:(fun oc ->
      List.iter functions ~f:(fun name ->
          fprintf oc "*(.text.caml.%s)\n" name))

let save_linker_script ~linker_script_template ~linker_script_hot
    ~output_filename =
  let output_filename =
    Option.value output_filename ~default:"linker-script"
  in
  let template =
    Option.value linker_script_template ~default:"linker-script-template"
  in
  if !verbose then (
    printf "Writing linker script to %s\n" output_filename;
    printf "Template %s\n" template );
  Out_channel.with_file output_filename ~f:(fun oc ->
      In_channel.with_file template
        ~f:
          (In_channel.iter_lines ~f:(fun line ->
               if
                 String.equal (String.strip line)
                   "INCLUDE linker-script-hot"
               then (
                 match linker_script_hot with
                 | None ->
                     if (* just remove the marker *)
                        !verbose then
                       printf
                         "No linker-script-hot provided, removing the \
                          marker from linker-script-template.\n"
                 | Some hot ->
                     if !verbose then printf "Hot function layout %s\n" hot;
                     In_channel.with_file hot
                       ~f:
                         (In_channel.iter_lines ~f:(fun s ->
                              fprintf oc "\t%s\n" s)) )
               else Out_channel.fprintf oc "%s\n" line)))

let decode ~binary_filename ~perf_profile_filename ~reorder_functions
    ~linker_script_hot_filename ~linearid_profile_filename
    ~write_linker_script =
  let locations = load_locations binary_filename in
  (* let dirname = Filename.(realpath (dirname binary_filename)) in *)
  let linearid_profile_filename =
    Option.value linearid_profile_filename
      ~default:(binary_filename ^ ".fdo-profile")
    (* dirname ^ "/fdo-profile" *)
  in
  (* First aggregate raw profile and then decode it. *)
  let aggr_perf_profile =
    Perf_profile.read_and_aggregate perf_profile_filename
  in
  let linearid_profile =
    Aggregated_decoded_profile.create locations aggr_perf_profile
  in
  load_crcs locations linearid_profile.crcs;

  (* Save the profile to file. This does not include counts for inferred
     fallthroughs. *)
  Aggregated_decoded_profile.write linearid_profile
    linearid_profile_filename;

  (* Create linker script *)
  if write_linker_script then (
    let linker_script_hot_filename =
      Option.value linker_script_hot_filename
        ~default:(binary_filename ^ ".linker-script-hot")
      (* The default name does not depend on the input executable, so that
         the default template link-script can refer to it and won't need to
         be updated or written per executable. *)
      (* binary_filename ^ ".linker-script-hot" *)
      (* dirname ^ "/linker-script-hot" *)
    in
    if !verbose then
      printf "Writing linker script hot to %s\n" linker_script_hot_filename;
    let open Config_reorder.Reorder_functions in
    match reorder_functions with
    | No -> ()
    | Execounts ->
        Aggregated_decoded_profile.top_functions linearid_profile
        |> save_linker_script_hot linker_script_hot_filename
    | Hot_clusters ->
        (* Do we ever need the cfg to decide on function order? *)
        failwith "Not implemented" );
  ()

let report_linear ~name title f =
  Report.with_ppf ~name ~title ~sub:"lin" Printlinear.fundecl f

let report_cfg ~name title cfg =
  Report.with_outchannel ~name ~title ~sub:"lin" (Print.debug_print "") cfg

exception Not_equal_reg_array

let reg_array_equal ra1 ra2 =
  let reg_equal r1 r2 =
    let open Reg in
    r1.stamp = r2.stamp
  in
  try
    Array.iter2_exn
      ~f:(fun r1 r2 ->
        if not (reg_equal r1 r2) then raise Not_equal_reg_array)
      ra1 ra2;
    true
  with Not_equal_reg_array -> false

let check_equal f ~new_body =
  let open Linear in
  let rec equal i1 i2 =
    (* Format.kasprintf prerr_endline "@;%a" Printlinear.instr i1;
     * Format.kasprintf prerr_endline "@;%a" Printlinear.instr i2; *)
    if
      i1.desc = i2.desc
      (* && i1.id = i2.id *)
      && reg_array_equal i1.arg i2.arg
      && reg_array_equal i1.res i2.res
      && Reg.Set.equal i1.live i2.live
      && Debuginfo.compare i1.dbg i2.dbg = 0
    then if i1.desc = Lend then true else equal i1.next i2.next
    else (
      Format.kasprintf prerr_endline "Equality failed in %s on:@;%a@;%a"
        f.fun_name Printlinear.instr i1 Printlinear.instr i2;
      false )
  in
  if not (equal f.fun_body new_body) then (
    let name = X86_proc.string_of_symbol "" f.fun_name in
    (* Separate files for before and after to make it easier to diff *)
    report_linear ~name "Before" f;
    report_linear ~name "After" { f with fun_body = new_body };
    if !strict then
      failwithf
        "Conversion from linear to cfg and back to linear is not an \
         identity function %s.\n"
        name () )

let optimize files ~fdo_profile ~reorder_blocks ~extra_debug ~unit_crc
    ~func_crc ~report =
  if report then Report.start ();
  let profile =
    match fdo_profile with
    | None -> None
    | Some filename -> Some (Aggregated_decoded_profile.read filename)
  in
  let algo =
    let open Config_reorder.Reorder_blocks in
    match reorder_blocks with
    | No -> Reorder.Identity
    | Random -> Reorder.Random (Random.State.make_self_init ())
    | Opt -> (
        match profile with
        | None ->
            failwith
              "-reorder-blocks opt is not allowed without -fdo-profile"
        | Some profile -> Reorder.Profile profile )
  in
  let crcs =
    match profile with
    | None -> Crcs.(mk Create)
    | Some profile -> Crcs.(mk (Compare profile.crcs))
  in
  let transform f =
    print_linear "Before" f;
    let cfg = Linear_to_cfg.run f ~preserve_orig_labels:false in
    (* eliminate fallthrough implies dead block elimination *)
    Eliminate.fallthrough_blocks cfg;
    let new_cfg = Reorder.apply ~algo cfg in
    let new_body = Cfg_to_linear.run new_cfg ~extra_debug in
    let fnew = { f with fun_body = new_body } in
    print_linear "After" fnew;
    fnew
  in
  let process file =
    (* CR-soon gyorsh: identify format based on the file extension and magic
       number. *)
    let out_filename = make_fdo_filename file in
    let open Linear_format in
    let ui, crc = restore file in
    let crc = Md5.of_hex_exn (Caml.Digest.to_hex crc) in
    if unit_crc then Crcs.add_unit crcs ~name:ui.unit_name crc ~file;
    ui.items <-
      List.map ui.items ~f:(function
        | Func f ->
            if func_crc then Crcs.add_fun crcs f ~file;
            Func (transform f)
        | Data dl -> Data dl);
    ( if extra_debug && (unit_crc || func_crc) then
      match Crcs.emit_symbols crcs with
      | [] -> ()
      | dl -> ui.items <- ui.items @ [ Data dl ] );
    save out_filename ui
  in
  List.iter files ~f:process;
  if report then Report.finish ()

(* CR-someday gyorsh: This is quite ad hoc and won't work with some args
   accepted by ocaml compiler.

   For example, if "-c -o foo.ml bar.ml" is passed, the call to ocamlopt
   from "ocamlfdo compile" might fail or do something unexpected, but it
   should probably be an error to use .ml extension of a output of
   compilation. Or for example, we might not know that it is
   compilation-only.

   It does not take into account additional ways to provide command line
   arguments, such as via environment variables or file. It should not be a
   problem for common and intended uses. Only input .ml files or -o <target>
   matter for "ocamlfdo compile". *)
let compile args ~fdo_profile ~reorder_blocks ~extra_debug ~unit_crc
    ~func_crc ~report =
  let open Wrapper in
  let open Ocaml_locations in
  let args = Option.value args ~default:[] in
  let src_files =
    (* change command line args to call ocamlopt *)
    match args with
    | [] ->
        if !verbose then printf "Missing compilation command\n";
        []
    | _ ->
        if !verbose then (
          printf "ocamlopt";
          List.iter ~f:(fun s -> printf " %s" s) args;
          printf "\n" );

        (* Compute the names of linear ir files from [args] *)
        List.filter_map args ~f:(fun s ->
            if is_filename Source s then Some s else None)
  in
  if stop_before_linear args then call_ocamlopt args None
  else
    match src_files with
    | [] -> call_ocamlopt args None
    | [ src ] ->
        (* If there is only one source file to compile and -o specifies the
           target explicitly, and "-c" or similar specifies compilation only
           mode (i.e., no linking), then the target name is used for the
           names of the intermediate artifacts. *)
        call_ocamlopt args (Some Compile);
        let target =
          if compilation_only args then
            Option.value (last_target args) ~default:src
          else src
        in
        let linear = make_filename Linear target in
        let files = [ linear ] in
        check_artifacts files;
        optimize files ~fdo_profile ~reorder_blocks ~extra_debug ~unit_crc
          ~func_crc ~report;
        let args =
          List.map args ~f:(fun s ->
              if s = src then make_fdo_filename linear else s)
        in
        call_ocamlopt args (Some Emit)
    | _ ->
        (* find -o <target> and remove it for the compilation phase, because
           it is incompatible with -c that is implied by -stop-after
           scheduling that we use for split compilation. *)
        let args_no_target = remove_targets args in
        call_ocamlopt args_no_target (Some Compile);
        let files = List.map ~f:(make_filename Linear) src_files in
        check_artifacts files;
        optimize files ~fdo_profile ~reorder_blocks ~extra_debug ~unit_crc
          ~func_crc ~report;

        (* replace all occurances of <src>.ml with <file>.cmir-linear-fdo,
           where <file> is <src> unless target was named explicitly in args
           with a single src file. *)
        let args =
          List.map args ~f:(fun s ->
              if is_filename Source s then
                make_filename Linear s |> make_fdo_filename
              else s)
        in
        call_ocamlopt args (Some Emit)

(* CR-soon gyorsh: this is intended as a report at source level and human
   readable form, like inlining report. Currently, just prints the IRs.
   Could share infrastructure with inlining report to map back to source
   when reordering involves inlined functions. CR-soon gyorsh: add separate
   "dump" flags for all passes in ocamlfdo, printing to stdout similarly to
   -dcmm -dlinear in the compiler, etc. *)
let write_reorder_report f c newf newc =
  if not (phys_equal c newc) then (
    (* Separate files for before and after make it easier to diff *)
    let name = X86_proc.string_of_symbol "" f.Linear.fun_name in
    report_linear ~name "Before-Reorder" f;
    report_linear ~name "After-Reorder" newf;
    report_cfg ~name "Before-Reorder" c;
    report_cfg ~name "After-Reorder" newc )

(* CR-soon gyorsh: If we eliminate dead blocks before a transformation then
   some algorithms might not apply because they rely on perf data based on
   original instructions. On the other hand, for iterative fdo, if we don't
   have counters for an instruction, we don't know if it's because it is
   cold or because it wasn't there in the binary at all as it was eliminated
   by dce. It probably doesn't matter for the final layout, if the
   heuristics are reasonable w.r.t. cold code, then dead is just very cold,
   but having less code to deal with when computing layout will be more
   efficient. *)

(* CR-soon gyorsh: call emit directly after optimize. the problem is
   restoring global settings (such as clflags) that were determined from
   command line and environment etc is currently very cumbersome and highly
   dependent on the compiler internals. *)

(* Command line options based on a variant type *)
(* print all variants in option's help string *)
module type Alt = sig
  type t

  val to_string : t -> string

  val all : t list

  val default : t
end

module AltFlag (M : Alt) = struct
  let of_string s = List.find_exn M.all ~f:(fun t -> s = M.to_string t)

  let alternatives heading =
    let names = List.map M.all ~f:M.to_string in
    let default = M.(to_string default) in
    sprintf "%s: %s (default: %s)" heading
      (String.concat ~sep:"," names)
      default

  let mk name ~doc =
    assert (not (List.contains_dup ~compare M.all));
    Command.Param.(
      flag name
        (optional_with_default M.default (Command.Arg_type.create of_string))
        ~doc:(alternatives doc))
end

module Commonflag = struct
  type t = {
    name : string;
    doc : string;
    aliases : string list;
  }

  let optional t =
    Command.Param.(
      flag ~aliases:t.aliases t.name (optional Filename.arg_type) ~doc:t.doc)

  let required t =
    Command.Param.(
      flag ~aliases:t.aliases t.name (required Filename.arg_type) ~doc:t.doc)

  let flag_binary_filename =
    {
      name = "-binary";
      doc = "filename elf binary to optimize";
      aliases = [];
    }

  let flag_perf_profile_filename =
    {
      name = "-perf-profile";
      doc = "perf.data output of perf record";
      aliases = [];
    }

  let flag_linearid_profile_filename =
    {
      name = "-fdo-profile";
      aliases = [ "-linearid-profile" ];
      doc = "filename decoded profile";
    }

  let flag_output_filename =
    { name = "-o"; doc = "filename output"; aliases = [] }

  let flag_linker_script_template_filename =
    {
      name = "-linker-script-template";
      doc = "filename linker script template";
      aliases = [];
    }

  let flag_linker_script_hot_filename =
    {
      name = "-linker-script-hot";
      doc = "filename hot functions layout for linker script";
      aliases = [];
    }
end

let flag_report =
  Command.Param.(
    flag "-fdo-report" no_arg
      ~doc:
        " emit .fdo.org files showing FDO decisions (e.g., blocks reordered)")

let flag_dot =
  Command.Param.(
    flag "-dot" no_arg ~doc:" emit CFG in .dot format for debug")

let flag_dot_show_instr =
  Command.Param.(
    flag "-dot-detailed" no_arg
      ~doc:" emit detailed CFG in .dot format for debug")

let flag_v =
  Command.Param.(flag "-verbose" ~aliases:[ "-v" ] no_arg ~doc:" verbose")

let flag_q = Command.Param.(flag "-q" no_arg ~doc:" quiet")

let flag_no_linker_script_hot =
  Command.Param.(
    flag "-no-linker-script-hot" no_arg
      ~doc:" do not generate hot functions layout for linker script")

let flag_extra_debug =
  Command.Param.(
    flag "-extra-debug" no_arg
      ~doc:
        " add extra debug info to generated code to enable profile decoding")

let flag_crc =
  Command.Param.(
    flag "-md5" no_arg
      ~doc:
        " use md5 to detect source changes at function and compilation \
         unit level (implies both -md5-unit and -md5-fun)")

let flag_unit_crc =
  Command.Param.(
    flag "-md5-unit" no_arg
      ~doc:" use md5 per compilation unit to detect source changes")

let flag_func_crc =
  Command.Param.(
    flag "-md5-fun" no_arg
      ~doc:" use md5 per function to detect source changes")

let anon_files =
  Command.Param.(anon (sequence ("input" %: Filename.arg_type)))

let flag_reorder_blocks =
  let module RB = AltFlag (Config_reorder.Reorder_blocks) in
  RB.mk "-reorder-blocks"
    ~doc:"heuristics for reordering basic blocks of a function"

let flag_reorder_functions =
  let module RF = AltFlag (Config_reorder.Reorder_functions) in
  RF.mk "-reorder-functions"
    ~doc:"heuristics used for function layout generated in linker script"

let decode_command =
  Command.basic
    ~summary:"Decode perf.data obtained from running the executable."
    ~readme:(fun () ->
      "\n\
       Workflow:\n\
       Build your executable with ocamlfdo to enable extra debug info\n\
       for low-level optimizations (currently, only linearize pass).See \
       other options for details.\n\
       Use Linux Perf to sample hardware execution counters using LBR:\n\
       $ perf record -e cycles:u -j any,u -o perf.data <prog.exe> <args..>\n\n\
       Decode the samples:\n\
       $ ocamlfdo decode -perf-profile <perf.data> -binary <prog.exe> \n\n\
       It will generate two files:\n\
       prog.exe.linker-script-hot is a linker script with hot function \
       reordering, if enabled.\n\
       prog.exe.fdo-profile is a profile.\n\
       The profile can be used to reoptimize the executable. See other \
       ocamlfdo options.\n\
       Without -fdo-profile, the default filename is \
       <binary_filename>.fdo-profile.\n\
      \       Without -linker-script-hot, the default filename is \
       <binary_filename>.linker-script-hot, unless -no-linker-script-hot \
       is given, in which case the tool does not\n\
      \  generate any linker-script-hot.\n\n\n")
    Command.Let_syntax.(
      let%map v = flag_v
      and q = flag_q
      and binary_filename = Commonflag.(required flag_binary_filename)
      and perf_profile_filename =
        Commonflag.(required flag_perf_profile_filename)
      and reorder_functions = flag_reorder_functions
      and linearid_profile_filename =
        Commonflag.(optional flag_linearid_profile_filename)
      and linker_script_hot_filename =
        Commonflag.(optional flag_linker_script_hot_filename)
      and no_linker_script_hot = flag_no_linker_script_hot in
      verbose := v;
      if q then quiet ();
      let write_linker_script = not no_linker_script_hot in
      fun () ->
        decode ~binary_filename ~perf_profile_filename ~reorder_functions
          ~linker_script_hot_filename ~linearid_profile_filename
          ~write_linker_script)

let opt_command =
  Command.basic
    ~summary:
      "Use a profile to optimize intermediate representation of the program."
    ~readme:(fun () ->
      "\n\
       For example:\n\
       $ ocamlfdo opt -fdo-profile myexe.fdo-profile foo.cmir-linear \
       bar.cmir-linear\n\
       reads a profile from myexe.fdo-profile file, uses it\n\
       to optimize foo.cmir-linear and bar.cmir-linear and save the result \
       to\n\
       foo.cmir-linear-fdo and bar.cmir-linear-fdo.\n\
       The intermediate representation .cmir-linear files can be obtained by\n\
       $ ocamlopt -save-ir-after linearize foo.ml bar.ml <other options>\n\
       The decoded profile can be obtained by running \"ocamlfdo decode\".")
    Command.Let_syntax.(
      let%map v = flag_v
      and q = flag_q
      and extra_debug = flag_extra_debug
      and fdo_profile = Commonflag.(optional flag_linearid_profile_filename)
      and reorder_blocks = flag_reorder_blocks
      and report = flag_report
      and unit_crc = flag_unit_crc
      and func_crc = flag_func_crc
      and crc = flag_crc
      and files = anon_files in
      verbose := v;
      if q then quiet ();
      let unit_crc = unit_crc || crc in
      let func_crc = func_crc || crc in
      if !verbose && List.is_empty files then printf "No input files\n";
      fun () ->
        optimize files ~fdo_profile ~reorder_blocks ~extra_debug ~unit_crc
          ~func_crc ~report)

let compile_command =
  Command.basic ~summary:"ocamlfdo wrapper to ocamlopt"
    ~readme:(fun () ->
      "\n\
       For example:\n\
       $ ocamlfdo compile -fdo-profile myexe.fdo-profile -- <standard \
       ocamlopt options including -o myexe>\n\
       is the same as the following 3 steps:\n\n\
       (1) invoke ocamlopt with \"-save-ir-after linearize -stop-after \
       linearize\" \n\
      \    in addition to the options specified after '--'.\n\
       (2) invoke 'ocamlfdo opt -fdo-profile myexe.fdo-profile' \n\
      \    with the list ofintermediate representation files .cmir-linear \
       produced in step (1).\n\
       (3) invoke 'ocamlopt -start-from emit'\n\
      \    with updated .cmir-linear files produced in step (2) \n\
      \    and the rest of the options specified after  '--'.\n\n\
       All the options provided to 'ocamlfdo compile' before and after \
       '--' are passed to 'ocamlfdo opt' and 'ocamlopt' respectively, \
       except file names which are adjusted according to the step.\n\n\
       This command turns ocamlfdo into a wrapper of ocamlopt that can be \
       used\n\
      \   as a drop-in replacement. It allows users to run ocamlfdo \
       directly,\n\
      \   without the need to modify their build process. The downside is \
       that\n\
      \   optimizing builds repeat a lot of redundant work.\n\n\
       Limitations: For linking, it assumes that the user of invokes \
       ocamlopt\n\
      \   with '-ccopt \"-Xlinker --script=linker-script\"' and that \
       linker-script\n\
      \   includes linker-script-hot or an alternative filename as \
       specified by\n\
      \   -linker-script option to 'ocamlfdo decode'. Note that \
       linker-script-hot\n\
      \   produced by 'ocamlfdo decode' is specific to Linux/GNU. It can be\n\
      \   manually transformed to other formats. Note also that using the \
       script\n\
      \   for function reordering on a different system requires ocaml \
       support for\n\
      \   named function sections (not available currently on macos and \
       windows).\n\n\n")
    Command.Let_syntax.(
      let%map v = flag_v
      and q = flag_q
      and extra_debug = flag_extra_debug
      and fdo_profile = Commonflag.(optional flag_linearid_profile_filename)
      and reorder_blocks = flag_reorder_blocks
      and report = flag_report
      and unit_crc = flag_unit_crc
      and func_crc = flag_func_crc
      and crc = flag_crc
      and args =
        Command.Param.(
          flag "--" escape
            ~doc:"ocamlopt_args standard options passed to ocamlopt")
      in
      verbose := v;
      if q then quiet ();
      let unit_crc = unit_crc || crc in
      let func_crc = func_crc || crc in
      fun () ->
        compile args ~fdo_profile ~reorder_blocks ~extra_debug ~unit_crc
          ~func_crc ~report)

let linker_script_command =
  Command.basic
    ~summary:
      "Create linker script from a template and a layout of hot functions."
    ~readme:(fun () ->
      "Insert the hot functions from linker-script-hot into the template \
       linker script, replacing the marker\n\
       INCLUDE linker-script-hot\n\n\
       A linker script can be obtained by running the command \"ld \
       --verbose\" and patching the output. See the default template, \
       distributed with the tool, in resources directory.\n\n\
       Use \"ocamlfdo decode\" to generate hot function layout.\n\
       This command performs a trivial transformation on the files.\n\
       It is useful when the linker runs from a different directory than \
       the one where the linker-script-hot file resides, such as when \
       ocamlfdo is used in a build system.\n\n\
       Without -linker-script-hot, the marker is simply removed.\n\
      \       Argument of -linker-script-hot option must be a file that \
       contains\n\
       a valid fragment of linker script syntax. It can be empty. If \
       section names\n\
      \        listed in the file do not exist during linking, they are \
       simply ignored\n\
      \        without a warning. It can happen when recompiling after \
       source code change,\n\
      \        the numbers at the end of the function symbols can change.\n")
    Command.Let_syntax.(
      let%map v = flag_v
      and q = flag_q
      and output_filename = Commonflag.(optional flag_output_filename)
      and linker_script_template =
        Commonflag.(optional flag_linker_script_template_filename)
      and linker_script_hot =
        Commonflag.(optional flag_linker_script_hot_filename)
      in
      verbose := v;
      if q then quiet ();
      fun () ->
        save_linker_script ~linker_script_template ~linker_script_hot
          ~output_filename)

let main_command =
  Command.group ~summary:"Feedback-directed optimizer for Ocaml"
    ~readme:(fun () ->
      "\n\
       decode: parses perf.data to generate a profile using debug info in \
       the executable. \n\
       opt: transforms intermediate IR using a profile\n\n\
       Important: ocamlfdo relies on compiler-libs and thus the same build \
       of ocamlopt must be used for building both ocamlfdo and the \
       executable.")
    [
      ("decode", decode_command);
      ("opt", opt_command);
      ("linker-script", linker_script_command);
      ("compile", compile_command);
    ]

let () = Command.run main_command
