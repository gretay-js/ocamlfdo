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
      (Config_reorder.linker_script_filename config "prelim");

    (* For testing, output the computed profile in bolt format *)
    let gen_bolt_fdata =
      Config_reorder.bolt_fdata_filename config "prelim"
    in
    Bolt_profile.save linearid_profile aggr_perf_profile
      ~filename:gen_bolt_fdata locations;

    (* For testing, output the computed profile in "decoded bolt" format. *)
    let gen_decoded_bolt =
      Config_reorder.bolt_decoded_filename config "prelim"
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
          Config_reorder.bolt_decoded_filename config "ref"
        in
        Decoded_bolt_profile.write decoded_bolt_profile
          ~filename:ref_decoded_bolt );

  linearid_profile

let decode config ~binary_filename ~perf_profile_filename ~reorder_functions
    ~write_linker_script =
  let locations = load_locations binary_filename in
  let config =
    {
      (Config_reorder.default (binary_filename ^ ".fdo-profile")) with
      reorder_functions;
      write_linker_script;
    }
  in
  let linearid_profile =
    setup_profile locations config ~perf_profile_filename
      ~bolt_profile_filename:None
  in
  (* Save the profile to file. This does not include counts for inferred
     fallthroughs. *)
  Aggregated_decoded_profile.write linearid_profile
    config.Config_reorder.linearid_profile_filename;

  (* Create linker script *)
  let open Config_reorder in
  if config.write_linker_script then (
    let linker_script_hot = linker_script_filename config "" in
    if !verbose then
      printf "Writing linker script hot to %s\n" linker_script_hot;
    match config.reorder_functions with
    | No ->
        if !verbose then
          printf
            "Reorder functions is not enabled.Cannot output linker script.\n"
    | Execounts ->
        Aggregated_decoded_profile.write_top_functions linearid_profile
          linker_script_hot
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
                    Reorder.Profile (linearid_profile, config, locations) )
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
                Reorder.Profile (linearid_profile, config, locations) ) )

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

let check_equal f ~new_body =
  let open Linearize in
  let rec equal i1 i2 =
    (* Format.kasprintf prerr_endline "@;%a" Printlinear.instr i1;
     * Format.kasprintf prerr_endline "@;%a" Printlinear.instr i2; *)
    if
      i1.desc = i2.desc && i1.id = i2.id
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
    Report.linear ~name "Before" f;
    Report.linear ~name "After" { f with fun_body = new_body };
    if !strict then
      failwithf
        "Conversion from linear to cfg and back to linear is not an \
         identity function %s.\n"
        name () )

let print_linear msg f =
  if true then
    if !verbose then (
      printf "%s processing %s\n" f.Linearize.fun_name msg;
      Format.kasprintf prerr_endline "@;%a" Printlinear.fundecl f )

let rec remove_discriminator = function
  | [] -> []
  | item :: t ->
      if Ocaml_locations.(is_filename Linear item.Debuginfo.dinfo_file) then
        t
      else item :: remove_discriminator t

let rec remove_linear_discriminator i =
  let open Linearize in
  match i.desc with
  | Lend -> i
  | _ ->
      {
        i with
        dbg = remove_discriminator i.dbg;
        next = remove_linear_discriminator i.next;
      }

let remove_linear_discriminators f =
  let open Linearize in
  {
    f with
    fun_dbg = remove_discriminator f.fun_dbg;
    fun_body = remove_linear_discriminator f.fun_body;
  }

let unmarshal filename =
  try
    let ic = In_channel.create filename in
    let f = Marshal.from_channel ic in
    In_channel.close ic;
    f
  with _ -> Misc.fatal_errorf "Failed to marshal to file %s" filename

let marshal filename f =
  try
    let oc = Out_channel.create filename in
    Marshal.to_channel oc f [ Marshal.Closures ];
    Out_channel.close oc
  with _ -> Misc.fatal_errorf "Failed to marshal to file %s" filename

(* unmarshal, transform, marshal the result. *)
let process_linear ~f files args = Misc.fatal_error "not implemented"

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

let optimize_linear _ = Misc.fatal_error "Not implemented"

(* CR gyorsh: this is intended as a report at source level and human
   readable form, like inlining report. Currently, just prints the IRs.
   Could share infrastructure with inlining report to map back to source
   when reordering involves inlined functions. CR gyorsh: add separate
   "dump" flags for all passes in ocamlfdo, printing to stdout similarly to
   -dcmm -dlinear in the compiler, etc. *)
let write_reorder_report f cfg newf newcfg =
  if not (phys_equal cfg newcfg) then (
    (* Separate files for before and after make it easier to diff *)
    let name = X86_proc.string_of_symbol "" f.Linearize.fun_name in
    Report.linear ~name "Before-Reorder" f;
    Report.linear ~name "After-Reorder" newf;
    Report.cfg ~name "Before-Reorder" cfg;
    Report.cfg ~name "After-Reorder" newcfg )

let main ~binary_filename ~perf_profile_filename ~raw_layout_filename
    ~rel_layout_filename ~linearid_layout_filename ~gen_rel_layout
    ~gen_linearid_layout ~random_order ~eliminate_dead_blocks
    ~eliminate_fallthrough_blocks ~remove_linear_ids ~reorder_report
    ~preserve_orig_labels ~gen_linearid_profile ~linearid_profile_filename
    ~bolt_profile_filename ~linear files args =
  let algo =
    setup_reorder ~binary_filename ~perf_profile_filename
      ~raw_layout_filename ~rel_layout_filename ~linearid_layout_filename
      ~random_order ~gen_linearid_layout ~gen_linearid_profile
      ~linearid_profile_filename ~bolt_profile_filename
  in
  let reorder = Reorder.reorder ~algo in
  let w_rel = Rel_layout.writer gen_rel_layout in
  let write_rel_layout new_cfg =
    let new_layout = Cfg_builder.get_layout new_cfg in
    let fun_name = Cfg_builder.get_name new_cfg in
    w_rel fun_name new_layout
  in
  let validate f ~new_body =
    match algo with
    | Reorder.Identity ->
        if
          eliminate_fallthrough_blocks || eliminate_dead_blocks
          || not preserve_orig_labels
        then ()
        else check_equal f ~new_body
    | _ -> ()
  in
  let transform f =
    print_linear "Before" f;
    let cfg = Cfg_builder.from_linear f ~preserve_orig_labels in
    let new_cfg = reorder cfg in
    write_rel_layout new_cfg;

    (* If we eliminate dead blocks before a transformation then some
       algorithms might not apply because they rely on perf data based on
       original instructions. *)
    if eliminate_fallthrough_blocks then
      (* implies dead block elimination *)
      Cfg_builder.eliminate_fallthrough_blocks new_cfg
    else if eliminate_dead_blocks then
      Cfg_builder.eliminate_dead_blocks new_cfg;
    let new_body = Cfg_builder.to_linear new_cfg in
    validate f ~new_body;
    let fnew = { f with fun_body = new_body } in
    if reorder_report then write_reorder_report f cfg fnew new_cfg;
    let fnew =
      if remove_linear_ids then remove_linear_discriminators fnew else fnew
      (* CR gyorsh: for iterative fdo, renumber the instructions with fresh
         linear ids, as we may have removed some instructions and introduced
         new ones, for example when a fallthrough turned into a jump after
         reorder.*)
    in
    print_linear "After" fnew;
    fnew
  in
  if linear then optimize_linear ~f:transform files args (* ~call_emit *)
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
    let arg_type = Command.Arg_type.create of_string in
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
    Command.Param.(flag t.name (optional Filename.arg_type) ~doc:t.doc)

  let required t =
    Command.Param.(flag t.name (required Filename.arg_type) ~doc:t.doc)

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
      doc = "filename use decoded perf profile";
    }
end

let flag_v =
  Command.Param.(flag "-verbose" ~aliases:[ "-v" ] no_arg ~doc:" verbose")

let flag_q = Command.Param.(flag "-q" no_arg ~doc:" quiet")

let flag_remove_linear_ids =
  Command.Param.(
    flag "-remove-linear-ids" no_arg
      ~doc:
        " remove extra debug info before optimizing the IR (iterative fdo)")

let flag_extra_debug =
  Command.Param.(
    flag "-extra-debug" no_arg
      ~doc:" add extra debug info for profile decoding")

let flag_extra_debug =
  Command.Param.(
    flag "-write-linker-script" no_arg
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

let old_command =
  Command.basic ~summary:"ocamlfdo wrapper to ocamlopt"
    ~readme:(fun () ->
      "\n\
       Build your program with ocamlfdo to enable extra debug info\n\
       for low-level optimizations (currently, only linearize pass):\n\
       $ ocamlfdo -- <standard ocamlopt options including -o prog.exe>\n\n\
       Collect perf profile with LBR information:\n\
       $ perf record -e cycles:u -j any,u -o perf.data <prog.exe> <args..>\n\n\
       Run ocamlfdo with exactly the same version of the source code and\n\
       options as above:\n\
       $ ocamlfdo -perf-profile <perf.data> -binary <prog.exe> -- \\\n\
      \           <standard ocamlopt options including -o prog.fdo.exe>\n\n\
       Important: ocamlfdo relies on compiler-libs and thus the build of \
       ocamlfdo must\n\
       match the build of ocamlopt used above.\n")
    Command.Let_syntax.(
      let%map_open v = flag_v
      and q = flag_q
      and remove_linear_ids = flag_remove_linear_ids
      and extra_debug = flag_extra_debug
      and eliminate_dead_blocks =
        flag "-edb" no_arg ~doc:" eliminate dead blocks"
      and eliminate_fallthrough_blocks =
        flag "-efb" no_arg
          ~doc:" eliminate fallthrough blocks, implies -edb"
      and preserve_orig_labels =
        flag "-preserve-orig-labels" no_arg
          ~doc:" do not eliminate labels (for validation)"
      and gen_rel_layout =
        flag "-gen-layout"
          (optional Filename.arg_type)
          ~doc:"filename generate relative layout and write to <filename>"
      and gen_linearid_layout =
        flag "-gen-linearid-layout"
          (optional Filename.arg_type)
          ~doc:"filename generate relative layout and write to <filename>"
      and binary_filename = Commonflag.(optional flag_binary_filename)
      and perf_profile_filename =
        Commonflag.(optional flag_perf_profile_filename)
      and bolt_profile_filename =
        flag "-bolt-profile"
          (optional Filename.arg_type)
          ~doc:"perf.fdata output of perf2bolt"
      and gen_linearid_profile =
        flag "-gen-linearid-profile"
          (optional Filename.arg_type)
          ~doc:"filename output decoded perf profile"
      and linearid_profile_filename =
        Commonflag.(optional flag_linearid_profile_filename)
      and raw_layout_filename =
        flag "-raw-layout"
          (optional Filename.arg_type)
          ~doc:"filename block layout for reordering: raw binary addresses"
      and rel_layout_filename =
        flag "-rel-layout"
          (optional Filename.arg_type)
          ~doc:
            "filename block layout for reordering relative to function \
             start,does not require -binary"
      and linearid_layout_filename =
        flag "-linearid-layout" ~aliases:[ "-layout" ]
          (optional Filename.arg_type)
          ~doc:
            "filename same as -rel-layout but uses linear id not cfg labels"
      and linear =
        flag "-linear" no_arg
          ~doc:" compile from linear IR instead of source"
      and reorder_report =
        flag "-reorder-report" no_arg
          ~doc:" Emit files showing the decisions"
      and reorder_blocks = flag_reorder_blocks
      and reorder_functions = flag_reorder_functions
      and files = anon_files
      and args =
        flag "--" escape
          ~doc:"ocamlopt_args standard options passed to opcamlopt"
      in
      if v then verbose := true;
      if q then verbose := false;
      if !verbose then Report.verbose := true;
      let random_order =
        match reorder_blocks with
        | Random -> true
        | _ -> false
      in
      (* CR gyorsh: use choose_one to reduce the mess below? *)
      if !verbose then (
        if
          preserve_orig_labels
          && (eliminate_dead_blocks || eliminate_fallthrough_blocks)
        then (
          printf "Warning: Ignoring -preserve-orig-labels.\n";
          printf "Incompatible with -edb and -efb\n" );
        if random_order then
          if
            (not (perf_profile_filename = None))
            || (not (raw_layout_filename = None))
            || (not (rel_layout_filename = None))
            || (not (linearid_layout_filename = None))
            || not (binary_filename = None)
          then (
            printf
              "Warning: Ignoring -perf-profile -raw-layout -layout \
               -linearid-layout -binary. ";
            printf "Incompatible with -random\n" );
        if binary_filename = None then (
          if
            (not (perf_profile_filename = None))
            || not (raw_layout_filename = None)
          then (
            printf "Warning: ignoring -raw_layout and -perf-profile. ";
            printf "Cannot use without -binary.\n" ) )
        else if perf_profile_filename = None && raw_layout_filename = None
        then (
          printf "Warning: Ignoring -binary. ";
          printf "Cannot use without -perf-profile or -raw-layout.\n" ) );
      fun () ->
        main ~binary_filename ~perf_profile_filename ~raw_layout_filename
          ~rel_layout_filename ~linearid_layout_filename ~gen_rel_layout
          ~gen_linearid_layout ~random_order ~eliminate_dead_blocks
          ~eliminate_fallthrough_blocks ~remove_linear_ids ~reorder_report
          ~preserve_orig_labels ~gen_linearid_profile
          ~linearid_profile_filename ~bolt_profile_filename ~linear files
          args)

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
      let%map_open v = flag_v
      and q = flag_q
      and binary_filename = Commonflag.(required flag_binary_filename)
      and perf_profile_filename =
        Commonflag.(required flag_perf_profile_filename)
      and reorder_functions = flag_reorder_functions in
      and linker_script = flag_write_linker_script in
      verbose := v;
      if q then verbose := false;
      fun () ->
        decode ~binary_filename ~perf_profile_filename ~reorder_functions ~write_linker_script)

let opt_command =
  Command.basic
    ~summary:"Use fdo profile to optimize an intermediate representation."
    ~readme:(fun () ->
      "\n\
       $ ocamlfdo opt -fdo-profile foo.linear -o foo.fdo.linear\n\
       Important: ocamlfdo relies on compiler-libs and thus the build of \
       ocamlfdo must\n\
       match the build of ocamlopt that produced the input file.\n")
    Command.Let_syntax.(
      let%map_open v = flag_v
      and q = flag_q
      and remove_linear_ids = flag_remove_linear_ids
      and extra_debug = flag_extra_debug
      and fdo_profile = Commonflag.(optional flag_linearid_profile_filename)
      and reorder_blocks = flag_reorder_blocks
      and files = anon_files in
      verbose := v;
      if q then verbose := false;

      transform files ~remove_linear_ids ~extra_debug ~fdo_profile
        ~reorder_blocks)

let split_command = main_command

let callback_command = main_command

let main_command =
  Command.group ~summary:"Feedback-directed optimizer for Ocaml"
    [ ("decode", decode_command);
      (* parse perf profile and decode it, possibly calling ocamlopt
         afterwards. *)
      ("opt", opt_command);
      (* optimize one input file *)
      ("main", split_command) (* split compilation *)
        ("reopt", callback_command)
      (* callback from ocamlopt *)
    ]

let () = Command.run main_command
