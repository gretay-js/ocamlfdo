open Core

let verbose = ref false

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

let _test elf_locations name =
  let (start,size) = Elf_locations.resolve_function elf_locations ~name in
  printf "Found %s at 0x%Lx size 0x%Lx\n" name start size

let load_locations ~binary_filename =
  let elf_locations = Elf_locations.create ~elf_executable:binary_filename in
  if !verbose then Elf_locations.print_dwarf elf_locations;
  elf_locations

let read_perf_data ~perf_profile_filename =
  match perf_profile_filename with
  | None -> ()
  | Some filename ->
    printf "Reading perf profile from %s\n" filename

(* CR gyorsh: it has to be named t for csv  fields ppx to work. move to separate
 module. *)
type t = {
  address : int64;
  position : int;
  new_position : int;
} [@@deriving fields, csv, compare, sexp]

type layout = t list [@@deriving sexp]

let print_layout_item i =
  Printf.printf "0x%Lx:%d:%d\n" i.address i.position i.new_position

let print_layout l =
  (* List.iter l ~f:print_layout_item; *)
  Printf.printf !"%{sexp:layout}\n" l

let read_layout ~layout_filename =
  match layout_filename with
  | None -> []
  | Some filename -> begin
      printf "Reading layout from %s\n" filename;
      let layout = csv_load ~separator:':' filename in
      if !verbose then print_layout layout;
      layout
    end

let decode_layout layout locations =
  List.iter layout ~f:(fun l ->
    let func =
      Elf_locations.function_at_pc locations ~program_counter:l.address in
    begin
      match func with
      | None -> Printf.printf "NOT FOUND\n"
      | Some func -> Printf.printf "Function %s\n" func;
    end;
    let loc =
      Elf_locations.resolve locations ~program_counter:l.address in
    print_layout_item l;
    match loc with
    | None -> Printf.printf "NOT FOUND\n"
    | Some (file,line) -> Printf.printf "%s:%d\n" file line)

let main ~binary_filename ~perf_profile_filename ~layout_filename args =
  printf "Optimizing %s\n" binary_filename;
  let locations = load_locations ~binary_filename in
  read_perf_data ~perf_profile_filename;
  let layout = read_layout ~layout_filename in
  decode_layout layout locations;
  begin match args with
  | None | Some [] -> printf "Missing compilation command\n"
  | Some args ->
    printf "ocamlopt ";
    List.iter ~f:(fun s -> printf " %s" s) args;
    printf "\n"
  end;
  (* set command line to args to call ocamlopt *)
  let args = (Array.of_list (Option.value args ~default:[])) in
  let len = Array.length args in
  let argc = Array.length Sys.argv in
  assert (len < argc);
  Array.blit ~src:args ~src_pos:0 ~dst:Sys.argv ~dst_pos:1 ~len;
  (* Can't resize args array. Fake missing arguments. *)
  Array.fill Sys.argv ~pos:(len+1) ~len:(argc-len-1) "-inlining-report";
  Optmain.main ()

let command =
  Command.basic
    ~summary:"Feedback-directed optimizer for Ocaml"
    ~readme:(fun () ->
      "
Build your program with -g to generate debug info and -xg to enable extra dwarf info
for low-level optimizations (currently, only linearize pass):
$ ocamlopt -g -xg <standard ocamlopt options> -o prog.exe

Collect perf profile with LBR information:
$ perf record -e cycles:u -j any,u -o perf.data <prog.exe> <args..>

Run ocamlfdo with exactly the same version of the source code and options as above:
$ ocamlfdo -perf-profile <perf.data> -binary <prog.exe> -- <standard ocamlopt options>

Important: ocamlfdo relies on compiler-libs and thus the build of ocamlfdo must
match the build of ocamlopt used above.
"
    )
    Command.Let_syntax.(
      let%map_open
        v = flag "-v" no_arg ~doc:" verbose"
      and binary_filename = flag "-binary" (required Filename.arg_type)
                              ~doc:"filename elf binary to optimize"
      and perf_profile_filename = flag "-perf-profile" (optional Filename.arg_type)
                                    ~doc:"perf.data output of perf record"
      and layout_filename = flag "-layout" (optional Filename.arg_type)
                              ~doc:"filename block layout to use for reordering"
      and args = flag "--" (escape)
                   ~doc:"ocamlopt_args standard options passed to opcamlopt"
      in
      if v then verbose := true;
      fun () -> main
                  ~binary_filename
                  ~perf_profile_filename
                  ~layout_filename
                  args)

let () = Command.run command
