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
open Ocamlcfg

let verbose = ref false

let strict = ref false

let time f x =
  let open Time in
  let start = now () in
  let fx = f x in
  let stop = now () in
  printf "Execution time: %s\n" (Span.to_string (abs_diff stop start));
  fx

let print_linear msg f =
  if false then
    if !verbose then (
      printf "%s processing %s\n" f.Linear.fun_name msg;
      Format.kasprintf prerr_endline "@;%a" Printlinear.fundecl f )

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
      | Some suffix ->
          let name, hex = String.rsplit2_exn suffix ~on in
          let crc = Md5.of_hex_exn hex in
          Hashtbl.add_exn tbl ~key:name ~data:crc)

let decode ~binary_filename ~perf_profile_filename ~reorder_functions
    ~linker_script_filename ~linearid_profile_filename ~write_linker_script
    =
  let open Config_reorder.Reorder_functions in
  let locations = load_locations binary_filename in
  let linearid_profile_filename =
    match linearid_profile_filename with
    | None -> binary_filename ^ ".fdo-profile"
    | Some f -> f
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
    let linker_script_filename =
      match linker_script_filename with
      | None -> binary_filename ^ ".linear-script-hot"
      | Some f -> f
    in
    if !verbose then
      printf "Writing linker script hot to %s\n" linker_script_filename;
    match reorder_functions with
    | No ->
        if !verbose then
          printf
            "Warning: reorder functions is not enabled. Writing top \
             functions anyway.\n";
        Aggregated_decoded_profile.write_top_functions linearid_profile
          linker_script_filename
    | Execounts ->
        Aggregated_decoded_profile.write_top_functions linearid_profile
          linker_script_filename
    | Hot_clusters ->
        (* Do we ever need the cfg to decide on function order? *)
        failwith "Not implemented" );
  ()

(* From 4.09/4.10 we should be able to use this hack instead of
   Obj.truncate. See https://github.com/ocaml/ocaml/pull/2279 *)

(* external caml_sys_modify_argv : string array -> unit =
 *   "caml_sys_modify_argv"
 *
 * let override_sys_argv new_argv =
 *   caml_sys_modify_argv new_argv;
 *   Arg.current := 0 *)

let override_sys_argv args =
  let len = Array.length args in
  assert (not (Array.length Sys.argv < len));
  Array.blit ~src:args ~src_pos:0 ~dst:Sys.argv ~dst_pos:0 ~len;
  Obj.truncate (Obj.repr Sys.argv) len

type phase =
  | Compile
  | Emit

let call_ocamlopt args phase =
  printf "call ocamlopt with %d args\n" (List.length args);

  (* Set debug "-g" to emit dwarf locations. *)
  ( match phase with
  | None -> ()
  | Some Compile ->
      if !verbose then printf "stage COMPILE\n";
      let open Clflags in
      stop_after := Some Compiler_pass.Linearize;
      set_save_ir_after Compiler_pass.Linearize true;
      debug := true;
      compile_only := true
  | Some Emit ->
      if !verbose then printf "stage EMIT\n";
      let open Clflags in
      stop_after := None;
      start_from := Some Compiler_pass.Emit;
      set_save_ir_after Compiler_pass.Linearize false;
      compile_only := false;
      debug := true );
  let argv = List.to_array (Sys.argv.(0) :: args) in
  if !verbose then (
    printf "calling ";
    Array.iter argv ~f:(fun s -> printf " %s" s);
    printf "\n" );

  override_sys_argv argv;
  Optmain.main ();
  printf "finished!\n"

let report_linear ~name title f =
  Report.with_ppf ~name ~title ~sub:"lin" Printlinear.fundecl f

let report_cfg ~name title cfg =
  Report.with_outchannel ~name ~title ~sub:"lin" Cfg_builder.print cfg

let check_equal f ~new_body =
  let open Linear in
  let rec equal i1 i2 =
    (* Format.kasprintf prerr_endline "@;%a" Printlinear.instr i1;
     * Format.kasprintf prerr_endline "@;%a" Printlinear.instr i2; *)
    if
      i1.desc = i2.desc
      (* && i1.id = i2.id *)
      && Reg.array_equal i1.arg i2.arg
      && Reg.array_equal i1.res i2.res
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
    ~func_crc ~report:_ =
  let algo, crcs =
    match fdo_profile with
    | None ->
        let algo =
          let open Config_reorder.Reorder_blocks in
          match reorder_blocks with
          | Random -> Reorder.Random (Random.State.make_self_init ())
          | No -> Reorder.Identity
          | Opt ->
              failwith
                "-reorder-blocks opt is not allowed without -fdo-profile"
        in
        let crcs = Crcs.(mk Create) in
        (algo, crcs)
    | Some fdo_profile ->
        let linearid_profile =
          Aggregated_decoded_profile.read fdo_profile
        in
        let config =
          {
            (Config_reorder.default (fdo_profile ^ ".new")) with
            reorder_blocks;
          }
        in
        let algo = Reorder.Profile (linearid_profile, config) in
        let crcs = Crcs.(mk (Compare linearid_profile.crcs)) in
        (algo, crcs)
  in
  let transform f =
    print_linear "Before" f;
    let cfg = Cfg_builder.from_linear f ~preserve_orig_labels:false in
    (* eliminate fallthrough implies dead block elimination *)
    Cfg_builder.eliminate_fallthrough_blocks cfg;
    let new_cfg = Reorder.reorder ~algo cfg in
    let new_body = Cfg_builder.to_linear new_cfg ~extra_debug in
    let fnew = { f with fun_body = new_body } in
    print_linear "After" fnew;
    fnew
  in
  let process file =
    (* CR gyorsh: identify format based on the file extension and magic
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
  List.iter files ~f:process

(* CR gyorsh: this is intended as a report at source level and human
   readable form, like inlining report. Currently, just prints the IRs.
   Could share infrastructure with inlining report to map back to source
   when reordering involves inlined functions. CR gyorsh: add separate
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

(* CR gyorsh: If we eliminate dead blocks before a transformation then some
   algorithms might not apply because they rely on perf data based on
   original instructions. On the other hand, for iterative fdo, if we don't
   have counters for an instruction, we don't know if it's because it is
   cold or because it wasn't there in the binary at all as it was eliminated
   by dce. It probably doesn't matter for the final layout, if the
   heuristics are reasonable w.r.t. cold code, then dead is just very cold,
   but having less code to deal with when computing layout will be more
   efficient. *)

(* CR gyorsh: call emit directly after optimize. the problem is restoring
   global settings (such as clflags) that were determined from command line
   and environment etc is currently very cumbersome and highly dependent on
   the compiler internals. *)
let compile args ~fdo_profile ~reorder_blocks ~extra_debug ~unit_crc
    ~func_crc ~report =
  let open Ocaml_locations in
  let files =
    (* check command line to args to call ocamlopt *)
    match args with
    | None | Some [] ->
        if !verbose then printf "Missing compilation command\n";
        []
    | Some args ->
        if !verbose then (
          printf "ocamlopt";
          List.iter ~f:(fun s -> printf " %s" s) args;
          printf "\n" );

        (* Compute the names of linear ir files from [args] *)
        List.filter_map args ~f:(fun s ->
            if is_filename Source s then Some (make_filename Linear s)
            else None)
  in
  let args = Option.value args ~default:[] in
  let finish () = Report.finish () in
  at_exit finish;
  ( match files with
  | [] -> call_ocamlopt args None
  | _ ->
      call_ocamlopt args (Some Compile);
      optimize [] (* files *) ~fdo_profile ~reorder_blocks ~extra_debug
        ~unit_crc ~func_crc ~report;
      let args =
        List.map args ~f:(fun s ->
            if is_filename Source s then
              make_filename Linear s |> make_fdo_filename
            else s)
      in
      call_ocamlopt args (Some Emit) );
  finish ()

(* Command line options based on a variant type *)
(* print all variants in option's help string *)
module type Alt = sig
  type t

  val to_string : t -> string

  val all : t list

  val default : t
end

module AltFlag (M : Alt) = struct
  let names =
    assert (not (List.contains_dup ~compare M.all));
    List.map M.all ~f:(fun m ->
        if m = M.default then M.to_string m ^ " (default)"
        else M.to_string m)

  let of_string s = List.find_exn M.all ~f:(fun t -> s = M.to_string t)

  let alternatives heading =
    sprintf "%s: %s" heading (String.concat ~sep:"," names)

  let mk name ~doc =
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
      name = "-linearid-profile";
      aliases = [ "-fdo-profile" ];
      doc = "filename decoded profile";
    }

  let flag_linker_script_filename =
    { name = "-linker-script"; doc = "filename link script"; aliases = [] }
end

let flag_report =
  Command.Param.(
    flag "-fdo-report" no_arg
      ~doc:
        " emit .fdo.org files showing FDO decisions (e.g., blocks reordered)")

let flag_v =
  Command.Param.(flag "-verbose" ~aliases:[ "-v" ] no_arg ~doc:" verbose")

let flag_q = Command.Param.(flag "-q" no_arg ~doc:" quiet")

let flag_no_linker_script =
  Command.Param.(
    flag "-no-linker-script" no_arg
      ~doc:" do not generate hot functions linker script")

let flag_extra_debug =
  Command.Param.(
    flag "-extra-debug" no_arg
      ~doc:
        " add extra debug info to generated code to enable profile decoding")

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
  RF.mk "-reorder-functions" ~doc:"heuristics for reordering functions"

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
       ocamlfdo options.\n")
    Command.Let_syntax.(
      let%map v = flag_v
      and q = flag_q
      and binary_filename = Commonflag.(required flag_binary_filename)
      and perf_profile_filename =
        Commonflag.(required flag_perf_profile_filename)
      and reorder_functions = flag_reorder_functions
      and linearid_profile_filename =
        Commonflag.(optional flag_linearid_profile_filename)
      and linker_script_filename =
        Commonflag.(optional flag_linker_script_filename)
      and no_linker_script = flag_no_linker_script in
      verbose := v;
      if q then verbose := false;
      let write_linker_script = not no_linker_script in
      fun () ->
        decode ~binary_filename ~perf_profile_filename ~reorder_functions
          ~linker_script_filename ~linearid_profile_filename
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
      and files = anon_files in
      verbose := v;
      if q then verbose := false;
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
      and args =
        Command.Param.(
          flag "--" escape
            ~doc:"ocamlopt_args standard options passed to ocamlopt")
      in
      verbose := v;
      if q then verbose := false;
      Reorder.verbose := !verbose;
      fun () ->
        compile args ~fdo_profile ~reorder_blocks ~extra_debug ~unit_crc
          ~func_crc ~report)

let main_command =
  Command.group ~summary:"Feedback-directed optimizer for Ocaml"
    ~readme:(fun () ->
      "\n\
       decode: parses perf.data to generate a profile using debug info in \
       the executable. \n\
       opt: transforms intermediate IR using a profile\n\
       compile: wrapper of ocamlopt that invokes \"ocamlfdo opt\"\n\
       Important: ocamlfdo relies on compiler-libs and thus the same build \
       of ocamlopt must be used for building both ocamlfdo and the \
       executable.")
    [
      ("decode", decode_command);
      ("opt", opt_command);
      ("compile", compile_command);
    ]

let () = Command.run main_command
