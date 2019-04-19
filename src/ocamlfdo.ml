open Core

let verbose = ref false

(* CR gyorsh: it has to be named t for csv  fields ppx to work. move to separate
   module. *)
(* We don't need all the fields, but redundancy is used for validating
   the input. *)
type t = {
  address : int64; (* start of function *)
  offset : int; (* offset from address *)
  index : int; (* index in the original layout *)
  position : int; (* new position in the permutation *)
} [@@deriving fields, csv, compare, sexp]

type permutation = t list [@@deriving sexp]

let _test elf_locations name =
  let (start,size) = Elf_locations.resolve_function elf_locations ~name in
  Printf.printf "Found %s at 0x%Lx size 0x%Lx\n" name start size

let load_locations binary_filename =
  let elf_locations = Elf_locations.create ~elf_executable:binary_filename in
  if !verbose then Elf_locations.print_dwarf elf_locations;
  elf_locations

let read_perf_data ~perf_profile_filename =
  match perf_profile_filename with
  | None -> ()
  | Some filename ->
    printf "Reading perf profile from %s\n" filename

let print_t i =
  Printf.printf "0x%Lx:0x%x:%d:%d\n" i.address i.offset i.position i.position

let print_permutation l =
  (* List.iter l ~f:print_t; *)
  Printf.printf !"%{sexp:permutation}\n" l

let read_permutation ~layout_filename =
  match layout_filename with
  | None -> []
  | Some filename -> begin
      printf "Reading layout from %s\n" filename;
      let p = csv_load ~separator:':' filename in
      if !verbose then print_permutation p;
      p
    end

let print_fun_layout_item (key, data) =
  Printf.printf "position=%d linear_id=%d\n" key data

let print_fun_layout ~key:name ~data:(fun_layout : (int,int) Hashtbl.t) =
  Printf.printf "%s (%d)\n" name (Hashtbl.length fun_layout);
  let sorted_fun_layout =
    List.sort (Hashtbl.to_alist fun_layout)
      ~compare:(fun (k1, _) (k2,_) -> Int.compare k1 k2)
  in
  List.iter sorted_fun_layout ~f:print_fun_layout_item

let print_layout layout =
  Hashtbl.iteri layout ~f:print_fun_layout

let to_func file =
  match String.chop_suffix file ~suffix:".linear" with
  | None ->
    Printf.printf "Ignoring %s\n" file;
    None
  | Some name ->
    let symbol_prefix =
      if X86_proc.system = X86_proc.S_macosx then "_" else ""
    in
    Some (X86_proc.string_of_symbol symbol_prefix name)

(* Use addresses from permutation locations to linear id layout *)
(* Linear ids that are not locations....  *)
let decode_layout permutation locations =
  let layout = Hashtbl.create (module String) in
  let add func pos id =
    let fun_layout =
      Hashtbl.find_or_add layout func
        ~default:(fun () -> Hashtbl.create (module Int)) in
    match Hashtbl.add fun_layout ~key:pos ~data:id with
    | `Duplicate -> Misc.fatal_errorf
                      "Cannot add linear_id %d at position %d in function %s"
                      id pos func
    | `Ok -> ()
  in
  let decode_item l =
    let func =
      Elf_locations.function_at_pc locations ~program_counter:l.address in
    begin
      match func with
      | None -> Printf.printf "NOT FOUND %Lx\n" l.address
      | Some func -> begin
          Printf.printf "Function %s\n" func;
          print_t l;
          let program_counter =
            Int64.(
              l.address + (Int64.of_int l.offset)) in
          let loc = Elf_locations.resolve locations ~program_counter in
          match loc with
          | None -> Printf.printf "Elf location NOT FOUND at 0x%Lx\n"
                      program_counter
          | Some (file,line) ->
            Printf.printf "%s:%d\n" file line;
            (* Check that the func symbol name from the binary where
               permutation comes from matches the function name encoded
               as filename into our special dwarf info. *)
            match to_func file with
            | None -> ()
            | Some func_name_dwarf -> begin
                if func_name_dwarf = func then
                  add func l.position line
                else
                  Misc.fatal_errorf
                    "func_name_dwarf = %s func = %s\n"
                    func_name_dwarf func
              end
        end
    end
  in
  List.iter permutation ~f:decode_item;
  if !verbose then print_layout layout;
  layout

(* let _register_passes () =
 *   let dump_linear_if = mk_pass_dump_if "dump_linear" Printlinear.fundecl in
 *   let blockreorder = mk_pass "blcok_reorder" Reorder.fundecl in
 *   let linear_invariants = mk_pass "linear_invariants" Linear_invariants.check
 *   in
 *   PassManager.register_before "scheduling" [ linear_invariants ];
 *   PassManager.register_before "emit"
 *     [ blockreorder;
 *       dump_linear_if dump_reorder "After block reordering";
 *       linear_invariants;
 *     ] *)

let setup_reoptimize ~binary_filename
      ~perf_profile_filename
      ~layout_filename
      ~random_order =
  match binary_filename with
  | None -> begin
      printf "Warning: missing -binary <filename>. Not running FDO\n";
      printf "Creating a binary for initial profiling.\n";
      (* Identity transformer, just generate linear ids and debug info. *)
      Reoptimize.set_transform ~f:(fun cfg -> cfg);
    end
  | Some binary_filename -> begin
      printf "Optimizing %s\n" binary_filename;
      if random_order then
        Reoptimize.set_transform ~f:(Reorder.reorder Reorder.Random)
      else begin
        let locations = load_locations binary_filename in
        let permutation = read_permutation ~layout_filename in
        if permutation <> [] then begin
          let layout = decode_layout permutation locations in
          if Hashtbl.is_empty layout then
            Misc.fatal_error "Cannot decode layout\n";
          Reoptimize.set_transform
            ~f:(fun cfg ->
               let cfg = Reorder.reorder (Reorder.External layout) cfg in
               (* If we eliminate dead blocks before a transformation
                  then some algorithms might not apply because they rely
                  on perf data based on original instructions. *)
               (* Cfg.eliminate_dead_blocks cfg; *)
               cfg
            );
        end
        else begin
          read_perf_data ~perf_profile_filename;
          Reoptimize.set_transform ~f:(Reorder.reorder Reorder.CachePlus)
        end;
      end;
    end

let call_ocamlopt args =
  Clflags.debug := true;
  (* set command line to args to call ocamlopt *)
  begin match args with
  | None | Some [] -> printf "Missing compilation command\n"
  | Some args ->
    printf "ocamlopt ";
    List.iter ~f:(fun s -> printf " %s" s) args;
    printf "\n"
  end;
  let args = (Array.of_list (Option.value args ~default:[])) in
  let len = Array.length args in
  let argc = Array.length Sys.argv in
  assert (len < argc);
  Array.blit ~src:args ~src_pos:0 ~dst:Sys.argv ~dst_pos:1 ~len;
  (* Can't resize args array. Fake missing arguments. *)
  Array.fill Sys.argv ~pos:(len+1) ~len:(argc-len-1) "-inlining-report";
  Optmain.main ()

let main ~binary_filename
      ~perf_profile_filename
      ~layout_filename
      ~random_order
      args =
  setup_reoptimize ~binary_filename
    ~perf_profile_filename ~layout_filename ~random_order;
  call_ocamlopt args

let command =
  Command.basic
    ~summary:"Feedback-directed optimizer for Ocaml"
    ~readme:(fun () ->
      "
Build your program with ocamlfdo to enable extra debug info
for low-level optimizations (currently, only linearize pass):
$ ocamlfdo -- <standard ocamlopt options including -o prog.exe>

Collect perf profile with LBR information:
$ perf record -e cycles:u -j any,u -o perf.data <prog.exe> <args..>

Run ocamlfdo with exactly the same version of the source code and options as above:
$ ocamlfdo -perf-profile <perf.data> -binary <prog.exe> -- \\
           <standard ocamlopt options including -o prog.fdo.exe>

Important: ocamlfdo relies on compiler-libs and thus the build of ocamlfdo must
match the build of ocamlopt used above.
"
    )
    Command.Let_syntax.(
      let%map_open
        v = flag "-v" no_arg ~doc:" verbose"
      and random_order = flag "-random-order" no_arg ~doc:" reorder blocks at random"
      and binary_filename = flag "-binary" (optional Filename.arg_type)
                              ~doc:"filename elf binary to optimize"
      and perf_profile_filename = flag "-perf-profile" (optional Filename.arg_type)
                                    ~doc:"perf.data output of perf record"
      and layout_filename = flag "-layout" (optional Filename.arg_type)
                              ~doc:"filename block layout to use for reordering"
      and args = flag "--" (escape)
                   ~doc:"ocamlopt_args standard options passed to opcamlopt"
      in
      if v then verbose := true;
      if random_order then begin
        if perf_profile_filename <> None then
          Printf.printf
            "Warning: Ignoring -perf-profile. Incompatible with -random-order\n";
        if layout_filename <> None then
          Printf.printf
            "Warning: Ignoring -layout. Incompatible with -random-order\n";
      end;
      fun () -> main
                  ~binary_filename
                  ~perf_profile_filename
                  ~layout_filename
                  ~random_order
                  args)

let () = Command.run command
