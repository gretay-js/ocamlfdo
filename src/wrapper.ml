(* CR-soon gyorsh: Most of this messing with command line arguments wouldn't
   be needed if we could invoke the compiler directly, as a (reentrant)
   library, or as a hook installed into the compiler using pass manager. *)
open Core

let verbose = ref true

(* artifact names for split compilation *)
type artifacts = {
  src_files : string list;
  target : string option;
  use_target_name_for_artifacts : bool;
}

type t = {
  args : string list;
  arts : artifacts option;
}

let get_files arts kind ~fdo =
  let f s =
    let open Ocaml_locations in
    let res = make_filename kind s in
    if fdo then make_fdo_filename res else res
  in
  if arts.use_target_name_for_artifacts then (
    assert (List.length src_files = 1);
    f arts.target )
  else List.map src_files ~f

type phase =
  | Compile
  | Emit

let call_ocamlopt w phase =
  let phase_flags =
    (* Set debug "-g" to emit dwarf locations. *)
    match phase with
    | None -> []
    | Some Compile ->
        if !verbose then printf "stage COMPILE\n";
        [
          "-g";
          "-stop-after";
          "scheduling";
          "-save-ir-after";
          "scheduling";
        ]
    | Some Emit ->
        if !verbose then printf "stage EMIT\n";
        [ "-g"; "-start-from"; "emit" ]
  in
  let args = args @ phase_flags in
  (* CR-soon gyorsh: how to get the correct path to ocamlopt, there may be
     multiple? *)
  let ocamlopt = "ocamlopt" in
  if !verbose then (
    printf "calling %s" ocamlopt;
    List.iter args ~f:(fun s -> printf " %s" s);
    printf "\n" );

  let open Shexp_process in
  let open Shexp_process.Infix in
  let ec = eval (spawn ocamlopt args >>= wait) in
  if !verbose then
    Printf.printf !"ocamlopt exit code = %{sexp:Exit_status.t}\n" ec;
  match ec with
  | Exited 0 -> ()
  | _ -> failwith "Call to ocamlopt failed"

(* CR-soon gyorsh: it's very inefficent to scan the args list over and over
   again for each argument. It can be done in one pass and starting to look
   more like command line parsing. *)
let rec remove_targets args =
  match args with
  | [] -> []
  | [ "-o" ] ->
      (* No argument after -o is likely to be an error when calling ocamlopt *)
      args
  | "-o" :: _ :: rest ->
      (* Check the rest for another -o argument. Currently, this is not an
         error and the compiler will use the last occurrence. *)
      remove_targets rest
  | arg :: rest -> arg :: remove_targets rest

let rec last_target = function
  | [] | [ _ ] -> None
  | "-o" :: target :: rest -> (
      (* Check the rest for another -o argument. Currently, this is not an
         error and the compiler will use the last occurrence. *)
      match last_target rest with
      | Some t -> Some t
      | None -> Some target )
  | _ :: rest -> last_target rest

let rec compilation_only = function
  | [] -> false
  | "-c" :: _ -> true
  | "-i" :: _ -> true
  | "-stop-after" :: pass :: rest ->
      if
        Clflags.Compiler_pass.(
          is_compilation_pass (Option.value_exn (of_string pass)))
      then true
      else compilation_only rest
  | _ :: rest -> compilation_only rest

let rec stop_before_linear = function
  | [] -> false
  | "-i" :: _ -> true
  | "-v" :: _ | "-version" :: _ -> true
  | _ :: rest -> stop_before_linear rest

(* stop if what you expected to exist doesn't exist. It's not a failure,
   because there are many ways in which ocamlopt may be invoked that stop
   before emit, but we don't want to confuse the user of ocamlfdo, and this
   way we will discover dynamically any situations that we need to handle.
   Some of this is done by the build system normally, so this shouldn't be
   needed when ocamlfdo is called from jenga/dune. *)
let check_artifacts files =
  let missing =
    List.filter files ~f:(fun f -> not (Sys.file_exists_exn f))
  in
  if not (List.is_empty missing) then (
    if !verbose then (
      printf "Missing files after the first phase of compilation: \n";
      List.iter missing ~f:(printf "%s\n");
      printf "\n" );
    assert false )

(* CR-someday gyorsh: This is quite ad hoc and won't work with some args
   accepted by ocaml compiler.

   For example, if "-c -o foo.ml bar.ml" is passed, the call to ocamlopt
   from "ocamlfdo compile" might fail or do something unexpected, but it
   should probably be an error to use .ml extension of a output of
   compilation. Or for example, we might not know that it is
   compilation-only.

   It does not take into account additional ways to provide command line
   arguments, such as via environment variables or file. It should not be a
   problem for common and intended uses. Only input .ml files or -o <target>
   matter for "ocamlfdo compile". *)
let wrap args =
  let open Ocaml_locations in
  let args = Option.value args ~default:[] in
  let src_files =
    (* change command line args to call ocamlopt *)
    match args with
    | [] ->
        if !verbose then printf "Missing compilation command\n";
        []
    | _ ->
        if !verbose then (
          printf "ocamlopt";
          List.iter ~f:(fun s -> printf " %s" s) args;
          printf "\n" );

        (* Compute the names of linear ir files from [args] *)
        List.filter_map args ~f:(fun s ->
            if is_filename Source s then Some s else None)
  in
  if stop_before_linear args then call_ocamlopt args None
  else
    match src_files with
    | [] -> call_ocamlopt args None
    | [ src ] ->
        (* If there is only one source file to compile and -o specifies the
           target explicitly, and "-c" or similar specifies compilation only
           mode (i.e., no linking), then the target name is used for the
           names of the intermediate artifacts. *)
        call_ocamlopt args (Some Compile);
        let target =
          if compilation_only args then
            Option.value (last_target args) ~default:src
          else src
        in
        let linear = make_filename Linear target in
        let files = [ linear ] in
        check_artifacts files;
        optimize files ~fdo_profile ~reorder_blocks ~extra_debug ~unit_crc
          ~func_crc ~report;
        let args =
          List.map args ~f:(fun s ->
              if s = src then make_fdo_filename linear else s)
        in
        call_ocamlopt args (Some Emit)
    | _ ->
        (* find -o <target> and remove it for the compilation phase, because
           it is incompatible with -c that is implied by -stop-after
           scheduling that we use for split compilation. *)
        let args_no_target = remove_targets args in
        call_ocamlopt args_no_target (Some Compile);
        let files = List.map ~f:(make_filename Linear) src_files in
        check_artifacts files;
        optimize files ~fdo_profile ~reorder_blocks ~extra_debug ~unit_crc
          ~func_crc ~report;

        (* replace all occurances of <src>.ml with <file>.cmir-linear-fdo,
           where <file> is <src> unless target was named explicitly in args
           with a single src file. *)
        let args =
          List.map args ~f:(fun s ->
              if is_filename Source s then
                make_filename Linear s |> make_fdo_filename
              else s)
        in
        call_ocamlopt args (Some Emit)
