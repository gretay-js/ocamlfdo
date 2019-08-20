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
open Layouts
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

let load_locations binary_filename =
  let elf_locations =
    Elf_locations.create ~elf_executable:binary_filename
  in
  if !verbose then Elf_locations.print_dwarf elf_locations;
  elf_locations

(* Symbols in the binary with a special naming sceme to communite crc of the
   linear IR it was compiled from. This is used to ensure that the profile
   is only applied to IR that it was obtained from.

   format: <crc_symbol_prefix><unit_name><crc_symbol_sep><crc_hex> example:
   caml_-_cRc_-_Ocamlfdo-123415810438DFE5 *)
let crc_symbol_prefix = "caml___cRc___"

let crc_symbol_sep = '_'

let crc_symbol_mk name crc =
  sprintf "%s%s%c%s" crc_symbol_prefix name crc_symbol_sep (Md5.to_hex crc)

let emit_crc_symbols crcs =
  let open Cmm in
  Hashtbl.fold crcs ~init:[] ~f:(fun ~key:name ~data:crc items ->
      let symbol = crc_symbol_mk name crc in
      Cglobal_symbol symbol :: Cdefine_symbol symbol :: items)

let load_crcs locations (linearid_profile : Aggregated_decoded_profile.t) =
  Elf_locations.iter_symbols locations ~prefix:crc_symbol_prefix
    ~f:(fun s ->
      let suffix = String.chop_prefix_exn s ~prefix:crc_symbol_prefix in
      let name, hex = String.rsplit2_exn s ~on:crc_symbol_sep in
      let crc = Md5.of_hex_exn hex in
      Hashtbl.add_exn linearid_profile.crcs ~key:name ~data:crc)

let setup_profile locations config ~perf_profile_filename
    ~bolt_profile_filename =
  (* CR gyorsh: decoding raw perf data should be separate from reordering
     algorithm setup. *)
  (* CR gyorsh: check buildid of the samples. Use owee? *)
  (* CR gyorsh: Check pid of the samples. *)

  (* First aggregate raw profile and then decode it. *)
  let aggr_perf_profile =
    Perf_profile.read_and_aggregate perf_profile_filename
  in
  let linearid_profile =
    Aggregated_decoded_profile.create locations aggr_perf_profile
  in
  if !verbose then (
    (* Save some info to files for testing *)
    (* CR gyorsh: write_top is here just for testing! *)
    (* in the case we want to use cfg info for function reordering. *)
    Aggregated_decoded_profile.write_top_functions linearid_profile
      (Config_reorder.get_linker_script_filename config "prelim");

    (* For testing, output the computed profile in bolt format *)
    let gen_bolt_fdata =
      Config_reorder.get_bolt_fdata_filename config "prelim"
    in
    Bolt_profile.save linearid_profile aggr_perf_profile
      ~filename:gen_bolt_fdata locations;

    (* For testing, output the computed profile in "decoded bolt" format. *)
    let gen_decoded_bolt =
      Config_reorder.get_bolt_decoded_filename config "prelim"
    in
    Decoded_bolt_profile.save linearid_profile aggr_perf_profile
      ~filename:gen_decoded_bolt;

    (* For testing, read in bolt format, decode, and write to file. *)
    match bolt_profile_filename with
    | None -> ()
    | Some bolt_profile_filename ->
        let decoded_bolt_profile =
          Decoded_bolt_profile.create ~filename:bolt_profile_filename
            locations
        in
        let ref_decoded_bolt =
          Config_reorder.get_bolt_decoded_filename config "ref"
        in
        Decoded_bolt_profile.write decoded_bolt_profile
          ~filename:ref_decoded_bolt );

  linearid_profile

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
  load_crcs locations linearid_profile;

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

type phase =
  | Compile
  | Emit

let call_ocamlopt args ~phase =
  (* Set debug "-g" to emit dwarf locations. *)
  let phase_flags =
    match phase with
    | None -> []
    | Some Compile ->
        [ "-stop-after"; "linearize"; "-save-ir-after"; "linearize" ]
    | Some Emit -> [ "-start-from"; "emit" ]
  in
  let argv = List.concat [ [ "-g" ]; args; phase_flags ] |> List.to_array in
  Optmain.main (* argv *) ()

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

let print_linear msg f =
  if !verbose then (
    printf "%s processing %s\n" f.Linear.fun_name msg;
    Format.kasprintf prerr_endline "@;%a" Printlinear.fundecl f )

let check_consistency_unit crcs name crc file ~if_not_found =
  Hashtbl.find_and_call crcs name
    ~if_found:(fun old_crc ->
      if not (Md5.equal old_crc crc) then
        failwithf
          "Linear IR for %s from file %s does not match the version of \
           this IR used for creating the profile.\n\
           old crc: %s\n\
           new crc: %s\n"
          name file (Md5.to_hex old_crc) (Md5.to_hex crc) ())
    ~if_not_found

let make_fdo_filename file = file ^ "-fdo"

let optimize files ~fdo_profile ~reorder_blocks ~extra_debug ~unit_crc
      ~func_crc =
  let options =
  match fdo_profile with
  | None ->
      let algo = Reorder.Identity in
      let crcs = Hashtbl.create (module String) in
      let check_unit_crc = unit_crc_find_or_add in
      let check_fun_crc = fun_crc_find_or_add in
      (algo, check_unit_crc, check_fun_crc, crc)
  | Some fdo_profile ->
      let linearid_profile = Aggregated_decoded_profile.read fdo_profile in
      let config =
        {
          (Config_reorder.default (fdo_profile ^ ".new")) with
          reorder_blocks;
        }
      in
      let algo = Reorder.Profile (linearid_profile, config) in
      let check_unit_crc = unit_crc_find_or_fail in
      let check_fun_crc = fun_crc_find_or_fail in
      let crcs = linearid_profile.crcs
  in
  let check_unit_crc = if unit_crc then check_unit_crc else Fun.id in
  let check_fun_crc = if func_crc then check_fun_crc else Fun.id in
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
    check_unit_crc ui.unit_name crc file;
    ui.items <-
      List.map ui.items ~f:(function
        | Func f -> Func (transform f)
        | Data dl -> Data dl);
    if extra_debug then
      ui.items <- ui.items @ [ Data (emit_crc_symbols crcs) ];
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
          printf "ocamlopt ";
          List.iter ~f:(fun s -> printf " %s" s) args;
          printf "\n" );

        (* Compute the names of linear ir files from [args] *)
        List.filter_map args ~f:(fun s ->
            if is_filename Source s then Some (make_filename Linear s)
            else None)
  in
  match files with
  | [] -> call_ocamlopt args None
  | _ ->
      call_ocamlopt args (Some compile);
      optimize files ~fdo_profile ~reorder_blocks ~extra_debug ~unit_crc
        ~func_crc ~report;
      let args =
        List.filter_map args ~f:(fun s ->
            if is_filename Source s then
              Some (make_filename Linear s |> make_fdo_filename)
            else Some s)
      in
      call_ocamlopt args (Some emit)

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
      \       $ ocamlfdo opt -fdo-profile myexe.fdo-profile \
       foo.cmir-linear bar.cmir-linear\n\
       reads a profile from myexe.fdo-profile file, uses it\n\
       to optimize foo.cmir-linear and bar.cmir-linear and save the result \
       to\n\
       foo.cmir-linear-fdo and bar.cmir-linear-fdo.\n\
       The intermediate representation .cmir-linear files can be obtained by\n\
       $ ocamlopt -save-ir-after linearize foo.ml bar.ml <other options>\n\
       The decoded profile can be obtained by running \"ocamlfdo decode\"")
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
       ocamlopt options including -o myexe>is the same as the following 3 \
       steps:\n\n\
      \        (1) invoke ocamlopt with \"-save-ir-after linearize \
       -stop-after linearize\" in addition to the options specified after \
       '--'.\n\
       (2) invoke 'ocamlfdo opt -fdo-profile myexe.fdo-profile' with the \
       list ofintermediate representation files .cmir-linear produced in \
       step (1).(3) invoke 'ocamlopt -start-from emit' with updated \
       .cmir-linear files produced\n\
      \            in step (2) and the rest of the options specified after \
       '--'.\n\
       All the options provided to 'ocamlfdo compile' before and after \
       '--' are passed to 'ocamlfdo opt' and 'ocamlopt' respectively, \
       except file names which are adjusted according to the step.")
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
        flag "--" escape
          ~doc:"ocamlopt_args standard options passed to ocamlopt"
      in
      verbose := v;
      if q then verbose := false;
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
