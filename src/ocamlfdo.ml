open Core

let verbose = ref false

let main perf_profile_filename binary_filename args =
  printf "Reading perf profile from %s\n" perf_profile_filename;
  printf "Optimizing %s\n" binary_filename;
  begin match args with
  | None | Some [] -> printf "Missing compilation command\n"
  | Some args ->
    printf "Built with";
    List.iter ~f:(fun s -> printf " %s" s) args;
    printf "\n"
  end;
  let open Elf_locations in
  let elf_locations = create ~elf_executable:binary_filename in
  print_dwarf elf_locations;
  let name = "camlTest2__foo_10" in
  let (start,size) = resolve_function elf_locations ~name in
  printf "Found %s at 0x%Lx size 0x%Lx\n" name start size;
  ()

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
