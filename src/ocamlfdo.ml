open Core

let verbose = ref false

let _test elf_locations name =
  let (start,size) = Elf_locations.resolve_function elf_locations ~name in
  printf "Found %s at 0x%Lx size 0x%Lx\n" name start size

let main perf_profile_filename binary_filename args =
  printf "Reading perf profile from %s\n" perf_profile_filename;
  printf "Optimizing %s\n" binary_filename;
  begin match args with
  | None | Some [] -> printf "Missing compilation command\n"
  | Some args ->
    printf "ocamlopt ";
    List.iter ~f:(fun s -> printf " %s" s) args;
    printf "\n"
  end;
  let elf_locations = Elf_locations.create ~elf_executable:binary_filename in
  if !verbose then Elf_locations.print_dwarf elf_locations;
  (* set command line to args to call ocamlopt *)
  let args = (Array.of_list (Option.value args ~default:[])) in
  let len = Array.length args in
  let argc = Array.length Sys.argv in
  assert (len < argc);
  Array.blit ~src:args ~src_pos:0 ~dst:Sys.argv ~dst_pos:1 ~len;
  (* Can't resize args array. Fake missing arguments. *)
  Array.fill Sys.argv ~pos:(len+1) ~len:(argc-len-1) "-inlining-report";
  try
    Optmain.main ()
  with _ ->
    printf "Exception from ocamlopt!\n";
    exit 1

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

Run ocamlfdo exactly the same version of the source code and options as above:
$ ocamlfdo -perf-profile <perf.data> -binary <prog.exe> -- <standard ocamlopt options>

Important: ocamlfdo relies on compiler-libs and thus the build of ocamlfdo must
match the build of ocamlopt used above.
"
    )
    Command.Let_syntax.(
      let%map_open
      v = flag "-v" no_arg ~doc:" verbose"
      and perf_profile_filename = flag "-perf-profile" (required file) ~doc:"perf.data output of perf record"
      and binary_filename = flag "-binary" (required file) ~doc:"filename elf binary to optimize"
      and args = flag "--" (escape) ~doc:"ocamlopt_args standard options passed to opcamlopt"
      in
      if v then verbose := true;
      fun () -> main perf_profile_filename binary_filename args)

let () = Command.run command
