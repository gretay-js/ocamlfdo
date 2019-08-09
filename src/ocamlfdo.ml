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

let setup_reorder ~binary_filename ~perf_profile_filename
    ~raw_layout_filename ~rel_layout_filename ~linearid_layout_filename
    ~random_order ~gen_linearid_layout ~gen_linearid_profile
    ~linearid_profile_filename ~bolt_profile_filename =
  (* CR: is there a better way to write all these cases?! *)
  if random_order then
    (* let random_state = Random.State.make [ deterministic seed ]; *)
    Reorder.Random Random.State.default
  else
    match binary_filename with
    | None -> (
        match rel_layout_filename with
        | Some rel_layout_filename ->
            let layout =
              convert_layout (Rel_layout.read rel_layout_filename)
            in
            Reorder.Cfg layout
        | None -> (
            match linearid_layout_filename with
            | Some linearid_layout_filename ->
                let layout =
                  convert_layout (Rel_layout.read linearid_layout_filename)
                in
                Reorder.Linear layout
            | None -> Reorder.Identity ) )
    | Some binary_filename -> (
        let locations = load_locations binary_filename in
        match raw_layout_filename with
        | Some raw_layout_filename ->
            let raw_layout = Raw_layout.read raw_layout_filename in
            let writer = Rel_layout.writer gen_linearid_layout in
            let layout = decode_layout_all locations raw_layout writer in
            Reorder.Linear layout
        | None -> (
            match perf_profile_filename with
            | None -> (
                match linearid_profile_filename with
                | None -> Reorder.Identity
                | Some linearid_profile_filename ->
                    let linearid_profile =
                      Aggregated_decoded_profile.read
                        linearid_profile_filename
                    in
                    let config =
                      Config_reorder.default
                        (linearid_profile_filename ^ ".new")
                    in
                    Reorder.Profile (linearid_profile, config) )
            | Some perf_profile_filename ->
                let linearid_profile_filename =
                  match gen_linearid_profile with
                  | None -> perf_profile_filename ^ ".linearid"
                  | Some f -> f
                in
                let config =
                  Config_reorder.default linearid_profile_filename
                in
                let linearid_profile =
                  setup_profile locations config ~perf_profile_filename
                    ~bolt_profile_filename
                in
                (* Passing locations only for testing bolt output. *)
                Reorder.Profile (linearid_profile, config) ) )

type phase =
  | Compile
  | Emit

let call_ocamlopt args ~phase =
  (* Set debug "-g" to emit dwarf locations. *)
  Clflags.debug := true;
  Clflags.extended_debug := true;

  (* set command line to args to call ocamlopt *)
  ( match args with
  | None | Some [] ->
      if !verbose then printf "Missing compilation command\n"
  | Some args ->
      if !verbose then (
        printf "ocamlopt ";
        List.iter ~f:(fun s -> printf " %s" s) args;
        printf "\n" ) );
  let args = Array.of_list (Option.value args ~default:[]) in
  let len = Array.length args in
  let argc = Array.length Sys.argv in
  assert (len < argc);
  Array.blit ~src:args ~src_pos:0 ~dst:Sys.argv ~dst_pos:1 ~len;

  (* CR gyorsh: Can't resize args array. Fake missing arguments. Better way? *)
  (* -inlining-report? *)
  Array.fill Sys.argv ~pos:(len + 1) ~len:(argc - len - 1) "-absname";

  let phase_flags =
    match phase with
    | None -> [||]
    | Some Compile ->
        [| "-stop-after"; "linearize"; "-save-ir-after"; "linearize" |]
    | Some Emit -> [| "-start-from"; "emit" |]
  in
  let _argv = Array.append Sys.argv phase_flags in
  (* CR change main to *)
  Optmain.main ()

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
  if true then
    if !verbose then (
      printf "%s processing %s\n" f.Linear.fun_name msg;
      Format.kasprintf prerr_endline "@;%a" Printlinear.fundecl f )

let process_linear ~f file =
  let open Linear in
  let linear_program = read file in
  Cmm.set_label linear_program.last_label;
  let items =
    List.map linear_program.items ~f:(function
      | Func d ->
          Func { d with decl = f d.decl }
          (* CR gyorsh: d.contains_calls may become inaccurate if an
             optimization deletes calls, for example dead code elimination,
             but we cannot recompute it, because it is target-dependent:
             selection.ml can redefine mark_call or mark_c_tailcall. *)
      | Data d -> Data d)
  in
  { linear_program with items }

let save file result =
  let filename = file ^ "-fdo" in
  Linear.write filename result

let process ~f file =
  (* CR gyorsh: identify format based on the file extension and magic
     number. *)
  process_linear ~f file |> save file

let optimize files ~fdo_profile ~reorder_blocks ~extra_debug =
  let algo =
    match fdo_profile with
    | None -> Reorder.Identity
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
        Reorder.Profile (linearid_profile, config)
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
  List.iter files ~f:(process ~f:transform)

(* (* Compute the list of input linear ir files from [files] and [args] *)
 * let open Ocaml_locations in
 * let inputs = List.filter files ~f:(is_filename Linear) in
 * (* CR gyorsh: two ways to use these variables *)
 * (List.filter_map args ~f:(fun s ->
 *    if is_filename Source s then
 *      Some (make_filename Linear s)
 *    else
 *      None
 *  ))
 * let process f =
 *   unmarshal f
 *   |> transform
 *   |> marshal (f^".fdo")
 * in
 * let linear_functions =
 *   List.map inputs ~f:process *)

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

(* If we eliminate dead blocks before a transformation then some algorithms
   might not apply because they rely on perf data based on original
   instructions. On the other hand, for iterative fdo, if we don't have
   counters for an instruction, we don't know if it's because it is cold or
   because it wasn't there in the binary at all as it was eliminated by dce.
   It probably doesn't matter for the final layout, if the heuristics are
   reasonable w.r.t. cold code, then dead is just very cold, but having less
   code to deal with when computing layout will be more efficient. *)
let main ~binary_filename ~perf_profile_filename ~raw_layout_filename
    ~rel_layout_filename ~linearid_layout_filename ~gen_rel_layout
    ~gen_linearid_layout ~random_order ~eliminate_dead_blocks
    ~eliminate_fallthrough_blocks ~extra_debug ~remove_linear_ids
    ~reorder_report ~preserve_orig_labels ~gen_linearid_profile
    ~linearid_profile_filename ~bolt_profile_filename files args =
  let algo =
    setup_reorder ~binary_filename ~perf_profile_filename
      ~raw_layout_filename ~rel_layout_filename ~linearid_layout_filename
      ~random_order ~gen_linearid_layout ~gen_linearid_profile
      ~linearid_profile_filename ~bolt_profile_filename
  in
  let w_rel = Rel_layout.writer gen_rel_layout in
  let write_rel_layout new_cfg =
    let new_layout = Cfg_builder.get_layout new_cfg in
    let fun_name = Cfg_builder.get_name new_cfg in
    w_rel fun_name new_layout
  in
  let transform f =
    let validate f new_cfg =
      match algo with
      | Reorder.Identity ->
          if
            eliminate_fallthrough_blocks || eliminate_dead_blocks
            || not preserve_orig_labels
          then ()
          else
            let new_body =
              Cfg_builder.to_linear new_cfg ~extra_debug:false
            in
            check_equal f ~new_body
      | _ -> ()
    in
    (* if the input already contains extra debug info, remove it. It may be
       needed for iterative fdo, which can add new extra debug info that is
       different and can be incompatible with the previous one. *)
    let f =
      if remove_linear_ids then Extra_debug.remove_linear_discriminators f
      else f
    in
    print_linear "Before" f;
    let cfg = Cfg_builder.from_linear f ~preserve_orig_labels in
    let new_cfg = Reorder.reorder ~algo cfg in
    write_rel_layout new_cfg;
    if eliminate_fallthrough_blocks then
      (* implies dead block elimination *)
      Cfg_builder.eliminate_fallthrough_blocks new_cfg
    else if eliminate_dead_blocks then
      Cfg_builder.eliminate_dead_blocks new_cfg;

    (* optionally validate by linearizing and comparing to the original *)
    validate f new_cfg;
    let new_body = Cfg_builder.to_linear new_cfg ~extra_debug in
    let fnew = { f with fun_body = new_body } in
    if reorder_report then write_reorder_report f cfg fnew new_cfg;
    print_linear "After" fnew;
    fnew
  in
  (* This is broken currently, we need to call split compilation here or
     something to check *)
  if not (List.is_empty files) then
    List.iter files ~f:(process ~f:transform)
  else (
    (* install a callback from the compiler *)
    Reoptimize.setup ~f:transform;
    let finish () =
      Reorder.finish algo;
      Report.finish ()
    in
    at_exit finish;
    call_ocamlopt args ~phase:None;
    finish () )

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
      doc = "filename decoded perf profile in this file";
    }

  let flag_gen_linearid_profile_filename =
    {
      name = "-linearid-profile";
      aliases = [ "-fdo-profile" ];
      doc = "filename decoded perf profile in this file";
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

let flag_remove_linear_ids =
  Command.Param.(
    flag "-remove-linear-ids" no_arg
      ~doc:
        " remove extra debug info before optimizing the IR (iterative fdo)")

let flag_extra_debug =
  Command.Param.(
    flag "-extra-debug" no_arg
      ~doc:" add extra debug info for profile decoding")

let anon_files =
  Command.Param.(anon (sequence ("input" %: Filename.arg_type)))

let flag_reorder_blocks =
  let module RB = AltFlag (Config_reorder.Reorder_blocks) in
  RB.mk "-reorder-blocks"
    ~doc:"heuristics for reordering basic blocks of a function"

let flag_reorder_functions =
  let module RF = AltFlag (Config_reorder.Reorder_functions) in
  RF.mk "-reorder-functions" ~doc:"heuristics for reordering functions"

(* let old_command =
 *   Command.basic ~summary:"ocamlfdo wrapper to ocamlopt"
 *     ~readme:(fun () ->
 *       "\n\
 *        Build your program with ocamlfdo to enable extra debug info\n\
 *        for low-level optimizations (currently, only linearize pass):\n\
 *        $ ocamlfdo -- <standard ocamlopt options including -o prog.exe>\n\n\
 *        Collect perf profile with LBR information:\n\
 *        $ perf record -e cycles:u -j any,u -o perf.data <prog.exe> <args..>\n\n\
 *        Run ocamlfdo with exactly the same version of the source code and\n\
 *        options as above:\n\
 *        $ ocamlfdo -perf-profile <perf.data> -binary <prog.exe> -- \\\n\
 *       \           <standard ocamlopt options including -o prog.fdo.exe>\n\n\
 *        Important: ocamlfdo relies on compiler-libs and thus the build of \
 *        ocamlfdo must\n\
 *        match the build of ocamlopt used above.\n")
 *     Command.Let_syntax.(
 *       let%map v = flag_v
 *       and q = flag_q
 *       and binary_filename = Commonflag.(optional flag_binary_filename)
 *       and perf_profile_filename =
 *         Commonflag.(optional flag_perf_profile_filename)
 *       and extra_debug = flag_extra_debug
 *       and gen_linearid_profile =
 *         Commonflag.(optional flag_gen_linearid_profile_filename)
 *       and linearid_profile_filename =
 *         Commonflag.(optional flag_linearid_profile_filename)
 *       and reorder_blocks = flag_reorder_blocks
 *       and reorder_functions = flag_reorder_functions
 *       and remove_linear_ids = flag_remove_linear_ids
 *       and files = anon_files
 *       and eliminate_dead_blocks =
 *         flag "-edb" no_arg ~doc:" eliminate dead blocks"
 *       and eliminate_fallthrough_blocks =
 *         flag "-efb" no_arg
 *           ~doc:" eliminate fallthrough blocks, implies -edb"
 *       and preserve_orig_labels =
 *         flag "-preserve-orig-labels" no_arg
 *           ~doc:" do not eliminate labels (for validation)"
 *       and gen_rel_layout =
 *         flag "-gen-layout"
 *           (optional Filename.arg_type)
 *           ~doc:"filename generate relative layout and write to <filename>"
 *       and gen_linearid_layout =
 *         flag "-gen-linearid-layout"
 *           (optional Filename.arg_type)
 *           ~doc:"filename generate relative layout and write to <filename>"
 *       and bolt_profile_filename =
 *         flag "-bolt-profile"
 *           (optional Filename.arg_type)
 *           ~doc:"perf.fdata output of perf2bolt"
 *       and raw_layout_filename =
 *         flag "-raw-layout"
 *           (optional Filename.arg_type)
 *           ~doc:"filename block layout for reordering: raw binary addresses"
 *       and rel_layout_filename =
 *         flag "-rel-layout"
 *           (optional Filename.arg_type)
 *           ~doc:
 *             "filename block layout for reordering relative to function \
 *              start,does not require -binary"
 *       and linearid_layout_filename =
 *         flag "-linearid-layout" ~aliases:[ "-layout" ]
 *           (optional Filename.arg_type)
 *           ~doc:
 *             "filename same as -rel-layout but uses linear id not cfg labels"
 *       and linear =
 *         flag "-linear" no_arg
 *           ~doc:" compile from linear IR instead of source"
 *       and reorder_report =
 *         flag "-reorder-report" no_arg
 *           ~doc:" Emit files showing the decisions"
 *       and args =
 *         flag "--" escape
 *           ~doc:"ocamlopt_args standard options passed to opcamlopt")
 *       in
 *       if v then verbose := true;
 *       if q then verbose := false;
 *       if !verbose then Report.verbose := true;
 *       let random_order =
 *         match reorder_blocks with
 *         | Random -> true
 *         | _ -> false
 *       in
 *       (* CR gyorsh: use choose_one to reduce the mess below? *)
 *       if !verbose then (
 *         if
 *           preserve_orig_labels
 *           && (eliminate_dead_blocks || eliminate_fallthrough_blocks)
 *         then (
 *           printf "Warning: Ignoring -preserve-orig-labels.\n";
 *           printf "Incompatible with -edb and -efb\n" );
 *         if random_order then
 *           if
 *             (not (perf_profile_filename = None))
 *             || (not (raw_layout_filename = None))
 *             || (not (rel_layout_filename = None))
 *             || (not (linearid_layout_filename = None))
 *             || not (binary_filename = None)
 *           then (
 *             printf
 *               "Warning: Ignoring -perf-profile -raw-layout -layout \
 *                -linearid-layout -binary. ";
 *             printf "Incompatible with -random\n" );
 *         if binary_filename = None then (
 *           if
 *             (not (perf_profile_filename = None))
 *             || not (raw_layout_filename = None)
 *           then (
 *             printf "Warning: ignoring -raw_layout and -perf-profile. ";
 *             printf "Cannot use without -binary.\n" ) )
 *         else if perf_profile_filename = None && raw_layout_filename = None
 *         then (
 *           printf "Warning: Ignoring -binary. ";
 *           printf "Cannot use without -perf-profile or -raw-layout.\n" ) );
 *       fun () ->
 *         main ~binary_filename ~perf_profile_filename ~raw_layout_filename
 *           ~rel_layout_filename ~linearid_layout_filename ~gen_rel_layout
 *           ~gen_linearid_layout ~random_order ~eliminate_dead_blocks
 *           ~eliminate_fallthrough_blocks ~remove_linear_ids ~reorder_report
 *           ~preserve_orig_labels ~gen_linearid_profile
 *           ~linearid_profile_filename ~bolt_profile_filename files args) *)

let decode_command =
  Command.basic
    ~summary:"Decode perf.data obtained from running the executable."
    ~readme:(fun () ->
      "\n\
       Build your program with ocamlfdo to enable extra debug info\n\
       for low-level optimizations (currently, only linearize pass):\n\
       $ ocamlfdo -- <standard ocamlopt options including -o prog.exe>\n\n\
       Collect perf profile with LBR information:\n\
       $ perf record -e cycles:u -j any,u -o perf.data <prog.exe> <args..>\n\n\
       $ ocamlfdo decode -perf-profile <perf.data> -binary <prog.exe> \n\n\
       It will generate two files:\n\
       prog.exe.linker-script is a linker script with hot function \
       reordering,\n\
       if enable.\n\
       prog.exe.fdo-profile is the aggregated decoded profile\n\
       The profile can be used to reoptimize the executable.\n\
       Run ocamlfdo with exactly the same version of the source code and\n\
       options as above.\n\
       $ ocamlfdo -fdo-profile prog.exe.fdo-profile -- \\\n\
       <standard ocamlopt options including -o prog.fdo.exe>\n\n\
       Important: ocamlfdo relies on compiler-libs and thus the build of \
       ocamlfdo must\n\
       match the build of ocamlopt used above.\n")
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
      "Use fdo profile to optimize intermediate representation of the \
       program."
    ~readme:(fun () ->
      "\n\
       For example:\n\
      \       $ ocamlfdo opt -fdo-profile myexe.fdo-profile foo.linear \
       bar.linear\n\
       reads decoded profile from myexe.fdo-profile file, uses it\n\
       to optimize foo.linear and bar.linear and save the result to\n\
       foo.fdo.linear and bar.fdo.linear.\n\
       The decoded profile can be obtained by running the command\n\
       $ ocamlfdo decode -binary myexe <other options>\n\
       The intermediate representation can be obtained by\n\
       $ ocamlopt -save-ir-after linearize foo.ml bar.ml <other options>\n\
       Important: ocamlfdo relies on compiler-libs and thus the build of \
       ocamlfdo must\n\
       match the build of ocamlopt that produced the intermediate \
       representation files.\n")
    Command.Let_syntax.(
      let%map v = flag_v
      and q = flag_q
      and extra_debug = flag_extra_debug
      and fdo_profile = Commonflag.(optional flag_linearid_profile_filename)
      and reorder_blocks = flag_reorder_blocks
      and files = anon_files in
      verbose := v;
      if q then verbose := false;
      if !verbose && List.is_empty files then printf "No input files\n";
      fun () -> optimize files ~fdo_profile ~reorder_blocks ~extra_debug)

let split_command =
  (* old_command *)
  Command.basic ~summary:"not implemented"
    Command.Let_syntax.(
      let%map v = flag_v in
      verbose := v;
      fun () -> ())

let callback_command =
  (* old_command *)
  Command.basic ~summary:"not implemented"
    Command.Let_syntax.(
      let%map v = flag_v in
      verbose := v;
      fun () -> ())

let main_command =
  Command.group ~summary:"Feedback-directed optimizer for Ocaml"
    ~readme:(fun () ->
      "\n\
       decode: parses perf.data to generate a profile using executable's \
       dwarf info\n\
       opt: transforms intermediate IR using a profile\n\
       compile: invokes ocamlopt's new split compilation\n\
       compile-with-callbacks: invokes ocamlopt with callbacks or hooks\n\n\
       Subcommands decode and opt are intended build systems (such as \
       jenga or dune)\n\
       to avoid redundant re-compilation.\n\
       Subcommands compile* are wrappers of ocamlopt intended for users\n\
       who invoke ocamlopt directly, and have lots of different options \
       for profile\n\
       generation and testing of various intermediate outputs.\n")
    [ ("decode", decode_command);
      ("opt", opt_command);
      ("compile", split_command);
      ("compile-with-callbacks", callback_command)
    ]

let () = Command.run main_command
