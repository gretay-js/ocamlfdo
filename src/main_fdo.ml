open Core
open Core.Poly
module CL = Ocamlcfg.Cfg_with_layout
module CP = Ocamlcfg.Passes

let verbosity_level = 20

let verbose = ref true

let strict = ref false

let time f x =
  let open Time in
  let start = now () in
  let fx = f x in
  let stop = now () in
  printf "Execution time: %s\n" (Span.to_string (abs_diff stop start));
  fx

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

let report_linear ~name title f =
  Report.with_ppf ~name ~title ~sub:"lin" Printlinear.fundecl f

let report_cfg ~name title cfg =
  Report.with_outchannel ~name ~title ~sub:"lin"
    (fun oc x -> CL.print x oc "")
    cfg

(* CR-soon gyorsh: this is intended as a report at source level and human
   readable form, like inlining report. Currently, just prints the IRs. Could
   share infrastructure with inlining report to map back to source when
   reordering involves inlined functions. CR-soon gyorsh: add separate "dump"
   flags for all passes in ocamlfdo, printing to stdout similarly to -dcmm
   -dlinear in the compiler, etc. *)
let write_reorder_report (f : Linear.fundecl) c newf newc =
  if not (phys_equal c newc) then (
    (* Separate files for before and after make it easier to diff *)
    let name = to_symbol f.fun_name in
    report_linear ~name "Before-Reorder" f;
    report_linear ~name "After-Reorder" newf;
    report_cfg ~name "Before-Reorder" c;
    report_cfg ~name "After-Reorder" newc )

let load_locations binary_filename =
  let elf_locations = Elf_locations.create ~elf_executable:binary_filename in
  if !verbose then Elf_locations.print_dwarf elf_locations;
  elf_locations

let decode ~binary_filename ~perf_profile_filename ~reorder_functions
    ~linker_script_hot_filename ~linearid_profile_filename
    ~write_linker_script_hot ~ignore_buildid ~expected_pids ~check
    ~write_aggregated_perf_profile ~read_aggregated_perf_profile =
  (* First aggregate raw profile and then decode it. *)
  let aggr_perf_profile =
    if read_aggregated_perf_profile then
      (* read pre-aggregated file, useful for debugging of decode *)
      Profile.record ~accumulate:true "read_aggregated"
        Aggregated_perf_profile.read perf_profile_filename
    else
      (* aggregate from perf.data *)
      let aggr_perf_profile =
        Profile.record_call ~accumulate:true "agg_perf_data" (fun () ->
            Perf_profile.read_and_aggregate perf_profile_filename
              binary_filename ~ignore_buildid expected_pids)
      in
      ( if write_aggregated_perf_profile then
        let filename =
          Option.value linearid_profile_filename
            ~default:(binary_filename ^ ".tmp.agg")
        in
        Aggregated_perf_profile.write aggr_perf_profile filename );
      aggr_perf_profile
  in
  let locations =
    Profile.record ~accumulate:true "load_elf_debug" load_locations
      binary_filename
  in
  let linearid_profile =
    Profile.record_call ~accumulate:true "decode" (fun () ->
        Aggregated_decoded_profile.create locations aggr_perf_profile)
  in
  (* Save the profile to file. This does not include counts for inferred
     fallthroughs, which require CFG. *)
  let linearid_profile_filename =
    Option.value linearid_profile_filename
      ~default:(binary_filename ^ ".fdo-profile")
    (* dirname ^ "/fdo-profile" *)
  in
  Aggregated_decoded_profile.write linearid_profile linearid_profile_filename;

  ( if write_linker_script_hot then
    let filename =
      Option.value linker_script_hot_filename
        ~default:(binary_filename ^ ".linker-script-hot")
    in
    Profile.record_call ~accumulate:true "linker_script_hot" (fun () ->
        Linker_script.write_hot filename ~linearid_profile ~reorder_functions
          ~check) );
  ()

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
let transform f ~algo ~extra_debug ~preserve_orig_labels =
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
        Reorder.apply ~algo cfg)
  in
  let new_body =
    Profile.record ~accumulate:true "cfg_to_linear" CL.to_linear new_cfg
  in
  let fnew = { f with fun_body = new_body } in
  print_linear "After" fnew;
  fnew

let optimize files ~fdo_profile ~reorder_blocks ~extra_debug ~unit_crc
    ~func_crc ~report =
  if report then Report.start ();
  let profile =
    match fdo_profile with
    | None -> None
    | Some filename ->
        let agg =
          Profile.record ~accumulate:true "read_fdo_profile"
            Aggregated_decoded_profile.read filename
        in
        Some agg
  in
  let algo =
    let open Config_reorder.Reorder_blocks in
    match reorder_blocks with
    | No -> Reorder.Identity
    | Random -> Reorder.Random (Random.State.make_self_init ())
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
  let crcs =
    match profile with
    | None -> Crcs.(mk Create)
    | Some profile -> Crcs.(mk (Compare profile.crcs))
  in
  let process file =
    (* CR-soon gyorsh: identify format based on the file extension and magic
       number. Only matters when fdo handles with more than one IR. *)
    let out_filename = Filenames.make_fdo file in
    let open Linear_format in
    let ui, crc = restore file in
    let crc = Md5.of_hex_exn (Caml.Digest.to_hex crc) in
    if unit_crc then Crcs.add_unit crcs ~name:ui.unit_name crc ~file;
    ui.items <-
      List.map ui.items ~f:(function
        | Func f ->
            if func_crc then Crcs.add_fun crcs f ~file;
            Func (transform f ~algo ~extra_debug ~preserve_orig_labels:false)
        | Data dl -> Data dl);
    ( if extra_debug && (unit_crc || func_crc) then
      match Crcs.emit_symbols crcs with
      | [] -> ()
      | dl -> ui.items <- ui.items @ [Data dl] );
    save out_filename ui
  in
  let process file =
    Profile.record_call ~accumulate:true "process" (fun () -> process file)
  in
  List.iter files ~f:process;
  if report then Report.finish ();
  ()

let merge files ~read_aggregated_perf_profile ~unit_crc ~func_crc
    ~ignore_buildid ~output_filename =
  if read_aggregated_perf_profile then
    Aggregated_perf_profile.M.merge_files files ~ignore_buildid
      ~output_filename
  else
    Aggregated_decoded_profile.M.merge_files files ~unit_crc ~func_crc
      ~ignore_buildid ~output_filename

let check files =
  let open Linear_format in
  let check_fun = function
    | Func f ->
        let fnew =
          transform f ~algo:Reorder.Identity ~extra_debug:false
            ~preserve_orig_labels:true
        in
        check_equal f fnew.fun_body
    | Data _ -> ()
  in
  let process file =
    printf "Checking %s...\n" file;
    let ui, _ = restore file in
    List.iter ui.items ~f:check_fun
  in
  List.iter files ~f:process

(* CR-soon gyorsh: call emit directly after optimize, working on the IR in
   memory instead of saving / restoring from file. The problem is set up
   global state (such as clflags) that were determined from command line and
   environment etc is currently very cumbersome and highly dependent on the
   compiler internals. There are multiple ways in which they can be set
   (e.g., from cmd line or from env or config), order matters, etc. *)
let compile args ~fdo_profile ~reorder_blocks ~extra_debug ~unit_crc
    ~func_crc ~report =
  let open Wrapper in
  let w = wrap args in
  if can_split_compile w then (
    Profile.record_call ~accumulate:true "phase I:compile" (fun () ->
        call_ocamlopt w Compile);
    let files = artifacts w Filenames.Linear in
    Profile.record_call ~accumulate:true "opt" (fun () ->
        optimize files ~fdo_profile ~reorder_blocks ~extra_debug ~unit_crc
          ~func_crc ~report);
    Profile.record_call ~accumulate:true "phase II:emit" (fun () ->
        call_ocamlopt w Emit) )
  else (
    if !verbose && (Option.is_some fdo_profile || extra_debug) then
      printf "Calling ocamlopt directly, without FDO.\n";
    call_ocamlopt w All )

let ocamlopt args =
  let open Wrapper in
  let w = wrap args in
  call_ocamlopt w All
