open Core
open Core.Poly
module CL = Ocamlcfg.Cfg_with_layout
module CP = Ocamlcfg.Passes
module AD = Aggregated_decoded_profile
module A = Aggregated_perf_profile

let verbosity_level = 20

let verbose = ref true

let strict = ref false

(* CR-soon gyorsh: This is the only machine dependent part. owee parser
   doesn't know how to handle /% that may appear in a function name, for
   example an Int operator. *)
let to_symbol name =
  let symbol_prefix =
    if X86_proc.system = X86_proc.S_macosx then "_" else ""
  in
  X86_proc.string_of_symbol symbol_prefix name

let print_linear msg f =
  if verbosity_level > 10 then
    if !verbose then (
      printf "%s processing %s\n" f.Linear.fun_name msg;
      Printlinear.fundecl Format.std_formatter f;
      Format.pp_print_flush Format.std_formatter () )

let merge files ~read_aggregated_perf_profile ~crc_config ~ignore_buildid
    ~output_filename =
  if read_aggregated_perf_profile then
    A.Merge.merge_files files ~crc_config ~ignore_buildid ~output_filename
  else
    AD.Merge.merge_files files ~crc_config ~ignore_buildid ~output_filename

let decode_file ~binary_filename ~perf_profile_filename ~reorder_functions
    ~linker_script_hot_filename ~output_filename ~write_linker_script_hot
    ~ignore_buildid ~expected_pids ~check ~write_aggregated_profile
    ~read_aggregated_perf_profile ~crc_config =
  let tmp ext = Filename.chop_extension output_filename ^ ".tmp" ^ ext in
  (* First aggregate raw profile and then decode it. *)
  let aggr_perf_profile =
    if read_aggregated_perf_profile then
      (* read pre-aggregated file, useful for debugging of decode *)
      Profile.record ~accumulate:true "read_aggregated" A.read
        perf_profile_filename
    else
      (* aggregate from perf.data *)
      let aggr_perf_profile =
        Profile.record_call ~accumulate:true "agg_perf_data" (fun () ->
            Perf_profile.read_and_aggregate perf_profile_filename
              binary_filename ignore_buildid expected_pids)
      in
      if write_aggregated_profile then A.write aggr_perf_profile (tmp ".agg");
      aggr_perf_profile
  in
  let locations =
    Profile.record_call ~accumulate:true "load_elf_debug" (fun () ->
        Elf_locations.create ~elf_executable:binary_filename)
  in
  let agg_dec_profile =
    Profile.record_call ~accumulate:true "decode" (fun () ->
        AD.create locations aggr_perf_profile ~crc_config)
  in
  if write_aggregated_profile then AD.write agg_dec_profile (tmp ".agg_dec");
  (* Save the profile to file. This does not include counts for inferred
     fallthroughs, which require CFG. *)
  AD.write_bin agg_dec_profile output_filename;

  ( if write_linker_script_hot then
    let filename =
      Option.value linker_script_hot_filename
        ~default:(binary_filename ^ ".linker-script-hot")
    in
    Profile.record_call ~accumulate:true "linker_script_hot" (fun () ->
        Linker_script.write_hot filename agg_dec_profile ~reorder_functions
          ~check) );
  ()

let decode files ~binary_filename ~reorder_functions
    ~linker_script_hot_filename ~output_filename ~write_linker_script_hot
    ~ignore_buildid ~expected_pids ~check ~write_aggregated_profile
    ~read_aggregated_perf_profile ~crc_config =
  let output_filename =
    Option.value output_filename ~default:(binary_filename ^ ".fdo-profile")
    (* dirname ^ "/fdo-profile" *)
  in
  match files with
  | [] -> if !verbose then Printf.printf "Missing input perf.data\n"
  | [perf_profile_filename] ->
      decode_file ~binary_filename ~perf_profile_filename ~reorder_functions
        ~linker_script_hot_filename ~output_filename ~write_linker_script_hot
        ~ignore_buildid ~expected_pids ~check ~write_aggregated_profile
        ~read_aggregated_perf_profile ~crc_config
  | _ ->
      let prefix = Filename.basename binary_filename in
      let tmp_files =
        List.map files ~f:(fun _ -> Filename.temp_file prefix ".fdo-profile")
      in
      List.iter2_exn files tmp_files ~f:(fun perf_profile_filename tmp ->
          decode_file ~binary_filename ~perf_profile_filename
            ~reorder_functions ~linker_script_hot_filename
            ~output_filename:tmp ~write_linker_script_hot:false
            ~ignore_buildid ~expected_pids ~check ~write_aggregated_profile
            ~read_aggregated_perf_profile ~crc_config);
      merge tmp_files ~read_aggregated_perf_profile:false ~crc_config
        ~ignore_buildid ~output_filename;
      if write_linker_script_hot then
        let agg_dec_profile = AD.read output_filename in
        let filename =
          Option.value linker_script_hot_filename
            ~default:(binary_filename ^ ".linker-script-hot")
        in
        Profile.record_call ~accumulate:true "linker_script_hot" (fun () ->
            Linker_script.write_hot filename agg_dec_profile
              ~reorder_functions ~check)

exception Not_equal_reg_array

let reg_array_equal ra1 ra2 =
  (* Uses stamps just like the implementation of compare for Reg.Set.t *)
  let reg_equal (r1 : Reg.t) (r2 : Reg.t) =
    if not (r1.stamp = r2.stamp) then raise Not_equal_reg_array
  in
  try
    Array.iter2_exn ~f:reg_equal ra1 ra2;
    true
  with Not_equal_reg_array -> false

let report_linear ~name title f =
  let filename = Report.get_filename ~name ~title ~sub:"lin" in
  let out_channel = Out_channel.create filename in
  let ppf = Format.formatter_of_out_channel out_channel in
  Misc.try_finally
    (fun () -> Printlinear.fundecl ppf f)
    ~always:(fun () ->
      Format.pp_print_flush ppf ();
      Out_channel.close out_channel)

let check_equal f new_body =
  (* CR-someday gyorsh: we do not preserve live and dbg fields of some
     instructions, such as labels, and instruction that do not exist in cfg,
     like trap handling stuff, or things that CFG can generate new ones.

     For live, this is fine because this field is used for Lop but not for
     labels and trap handling instruciont. For dbg, it is fine for now
     because mshinwell said so.*)
  let ignored = function
    | Linear.Llabel _ | Lentertrap | Ladjust_trap_depth _ -> true
    | _ -> false
  in
  let open Linear in
  let rec equal i1 i2 =
    if
      i1.desc = i2.desc
      && reg_array_equal i1.arg i2.arg
      && reg_array_equal i1.res i2.res
      && ( ignored i1.desc
         || Reg.Set.equal i1.live i2.live
            && Debuginfo.compare i1.dbg i2.dbg = 0 )
    then if i1.desc = Lend then true else equal i1.next i2.next
    else (
      Format.kasprintf prerr_endline "Equality failed in %s on:@;%a@;%a"
        f.fun_name Printlinear.instr i1 Printlinear.instr i2;
      false )
  in
  if not (equal f.fun_body new_body) then (
    let name = to_symbol f.fun_name in
    (* Separate files for before and after to make it easier to diff *)
    report_linear ~name "Before" f;
    report_linear ~name "After" { f with fun_body = new_body };
    if !strict then
      Report.user_error
        "Conversion from linear to cfg and back to linear is not an \
         identity function %s.\n"
        name () )

(* CR-soon gyorsh: If we eliminate dead blocks before a transformation then
   some algorithms might not apply because they rely on perf data based on
   original instructions. On the other hand, for iterative fdo, if we don't
   have counters for an instruction, we don't know if it's because it is cold
   or because it wasn't there in the binary at all as it was eliminated by
   dce. It probably doesn't matter for the final layout, if the heuristics
   are reasonable w.r.t. cold code, then dead is just very cold, but having
   less code to deal with when computing layout will be more efficient. *)
let transform f ~algo ~extra_debug ~preserve_orig_labels ~alternatives =
  print_linear "Before" f;
  let cfg =
    Profile.record_call ~accumulate:true "linear_to_cfg" (fun () ->
        CL.of_linear f ~preserve_orig_labels)
  in
  (* eliminate fallthrough implies dead block elimination *)
  if not preserve_orig_labels then
    Profile.record ~accumulate:true "eliminate_fallthrough"
      CL.eliminate_fallthrough_blocks cfg;
  ( if extra_debug then
    let file = to_symbol f.fun_name |> Filenames.(make Linear) in
    Profile.record_call ~accumulate:true "extra_debug" (fun () ->
        CP.add_extra_debug (CL.cfg cfg) ~file) );
  let new_cfg =
    Profile.record_call ~accumulate:true "reorder" (fun () ->
        Reorder.apply ~algo cfg ~alternatives)
  in
  let new_body =
    Profile.record ~accumulate:true "cfg_to_linear" CL.to_linear new_cfg
  in
  let fnew = { f with fun_body = new_body } in
  print_linear "After" fnew;
  fnew

let emit_crcs ui crcs =
  let open Cmir_format in
  (* emit crc symbols *)
  (* CR-someday gyorsh: emit into a separate data section, not interleaved
     with normal data items. We need to add named data sections to Cmm. In
     emit, change looks like this: D.section [".data.ocamlfdo.crcs"] (Some
     "") [ "%note" ]; Not sure about the "note" but the goal is to make it
     non-allocatable, so that it is not loa. *)
  (* CR-someday gyorsh: can we use elf notes format instead of creating all
     these useless symbols? D.section [".note.ocamlfdo"] (Some "") [ "%note"
     ]; Can owee read notes sections? *)
  let syms = Crcs.encode crcs in
  let dl =
    List.fold syms ~init:[] ~f:(fun items symbol ->
        let open Cmm in
        Cglobal_symbol symbol :: Cdefine_symbol symbol :: items)
  in
  ui.items <- ui.items @ [Data dl]

let optimize files ~fdo_profile ~reorder_blocks ~extra_debug ~crc_config
    ~report =
  if report then (
    Report.start ();
    Report.logf
      !"Optimizing %{sexp:string list}\n%s\nextra_debug=%b\n"
      files
      (Option.value fdo_profile ~default:"no profile")
      extra_debug );
  let profile =
    match fdo_profile with
    | None -> None
    | Some filename ->
        let agg =
          Profile.record ~accumulate:true "read_fdo_profile" AD.read_bin
            filename
        in
        Some agg
  in
  let algo =
    let open Config_reorder.Reorder_blocks in
    match reorder_blocks with
    | No -> Reorder.Identity
    | Random -> Reorder.Random Random.State.default
    | Opt -> (
        match profile with
        | None ->
            if !verbose then
              (* This is not an error so that options passed to opt by jenga
                 rules with and without the profile can be overwritten from
                 one environment variable. *)
              Printf.printf
                "Ignoring -reorder-blocks opt : not allowed without \
                 -fdo-profile \n";
            Reorder.Identity
        | Some profile -> Reorder.Profile profile )
  in
  let process file =
    (* CR-soon gyorsh: all crcs of previously processed files get printed for
       each file. *)
    let crcs =
      match profile with
      | None -> Crcs.(mk Create crc_config)
      | Some profile -> Crcs.(mk (Compare profile.crcs) crc_config)
    in

    (* CR-someday gyorsh: identify format based on the file extension and
       magic number. Only matters when fdo handles with more than one IR.

       CR-someday gyorsh: 4.11 will have a nicer interface to magic numbers,
       use it. *)
    let out_filename = Filenames.make_fdo file in
    let open Cmir_format in
    let ui, crc = restore ~filename:file ~magic:Config.linear_magic_number in
    if Crcs.add_unit crcs ui ~hex:(Caml.Digest.to_hex crc) ~file then (
      let skipped = ref 0 in
      let total = ref 0 in
      let new_items =
        List.map ui.items ~f:(fun item ->
            match item with
            | Data dl -> Data dl
            | Func f -> (
                incr total;
                let transform ~alternatives =
                  Func
                    (transform f ~algo ~extra_debug
                       ~preserve_orig_labels:false ~alternatives)
                in
                match Crcs.add_fun crcs f ~file with
                | false ->
                    incr skipped;
                    item
                | true -> transform ~alternatives:[]
                | exception Crcs.Near_match alternatives ->
                    transform ~alternatives ))
      in
      if !skipped > 0 then
        Report.logf
          "Skipped %d functions out of %d in compilation unit %s (%.3f)\n"
          !skipped !total file
          (Report.percent !skipped !total);
      ui.items <- new_items );
    if extra_debug then emit_crcs ui crcs;
    save ~filename:out_filename ~magic:Config.linear_magic_number ui
  in
  let process file =
    Profile.record_call ~accumulate:true "process" (fun () -> process file)
  in
  List.iter files ~f:process;
  Crcs.Config.report crc_config;
  ()

let check_mach files =
  let open Cmir_format in
  let transform mach =
    (* mach -> cfg -> linear  *)
    print_mach "Before" mach;
    let m_cfg = CL.of_mach mach ~preserve_orig_labels in
    let new_body = CL.to_linear m_cfg  in
    let fnew = { f with fun_body = new_body } in
    print_linear "After" fnew;
    (* mach -> linear -> cfg -> linear :
       the last two steps are to get rid of any dead labels *)
    let linear = Linearize.fundecl mach in
    print_linear "Before" linear;
    let l_cfg = CL.of_linear linear ~preserve_orig_labels in
    let expected = (CL.to_linear l_cfg) in
    (* compare *)
    check_equal fnew expected
  in
  let check_item = function
    | Func f ->
        check f
    | Data _ -> ()
  in
  let process filename =
    printf "Checking %s...\n" filename;
    let ui, _ = restore ~filename ~magic:Config.mach_magic_number in
    List.iter ui.items ~f:check_item
  in
  List.iter files ~f:process

let check files =
  let open Cmir_format in
  let check_item = function
    | Func f ->
        let fnew =
          transform f ~algo:Reorder.Identity ~extra_debug:false
            ~preserve_orig_labels:true ~alternatives:[]
        in
        check_equal f fnew.fun_body
    | Data _ -> ()
  in
  let process filename =
    printf "Checking %s...\n" filename;
    let ui, _ = restore ~filename ~magic:Config.linear_magic_number in
    List.iter ui.items ~f:check_item
  in
  List.iter files ~f:process

let dump files ~dot ~show_instr =
  let open Cmir_format in
  let dump_linear _oc ppf = function
    | Func f -> Printlinear.fundecl ppf f
    | Data dl -> Printcmm.data ppf dl
  in
  let dump_cfg oc _ppf = function
    | Data _ -> ()
    | Func f ->
        let cl = CL.of_linear f ~preserve_orig_labels:false in
        if dot then CL.print_dot ~show_instr cl "";
        CL.print cl oc ""
  in
  let process filename =
    printf "Dumping %s...\n" filename;
    let ui, _ = restore ~filename ~magic:Config.linear_magic_number in
    let dump_format ext dump_item =
      let dump_filename = sprintf "%s.dump.%s" filename ext in
      Out_channel.with_file dump_filename ~f:(fun oc ->
          let ppf = Format.formatter_of_out_channel oc in
          List.iter ui.items ~f:(dump_item oc ppf))
    in
    dump_format "linear" dump_linear;
    dump_format "cfg" dump_cfg
  in
  List.iter files ~f:process

(* CR-soon gyorsh: call emit directly after optimize, working on the IR in
   memory instead of saving / restoring from file. The problem is set up
   global state (such as clflags) that were determined from command line and
   environment etc is currently very cumbersome and highly dependent on the
   compiler internals. There are multiple ways in which they can be set
   (e.g., from cmd line or from env or config), order matters, etc. *)
let compile args ~fdo_profile ~reorder_blocks ~extra_debug ~crc_config
    ~report =
  let open Wrapper in
  let w = wrap args in
  if can_split_compile w then (
    Profile.record_call ~accumulate:true "phase I:compile" (fun () ->
        call_ocamlopt w Compile);
    let files = artifacts w Filenames.Linear in
    Profile.record_call ~accumulate:true "opt" (fun () ->
        optimize files ~fdo_profile ~reorder_blocks ~extra_debug ~crc_config
          ~report);
    Profile.record_call ~accumulate:true "phase II:emit" (fun () ->
        call_ocamlopt w Emit) )
  else (
    if !verbose && (Option.is_some fdo_profile || extra_debug) then
      printf "Calling ocamlopt directly, without FDO.\n";
    call_ocamlopt w All )
