(* CR-soon gyorsh: Most of this messing with command line arguments wouldn't
   be needed if we could invoke the compiler directly, as a (reentrant)
   library, or as a hook installed into the compiler using pass manager. *)
open Core

let verbose = ref true

(* artifact names for split compilation *)
type artifacts =
  | Stop_before_linear
  | Standard of string list
  | Single_src_rename of {
      src : string;
      target : string;
    }

type t = {
  args : string list;
  arts : artifacts;
}

let can_split_compile t =
  match t.arts with
  | Stop_before_linear -> false
  | Standard [] -> false
  | Standard _ -> true
  | Single_src_rename _ -> true

let rec remove_targets = function
  | ([] | [ _ ]) as args -> args
  | "-o" :: _ :: rest -> remove_targets rest
  | arg :: rest -> arg :: remove_targets rest

(* stop if what you expected to exist doesn't exist. It's not a failure,
   because there are many ways in which ocamlopt may be invoked that stop
   before emit, but we don't want to confuse the user of ocamlfdo, and this
   way we will discover dynamically any situations that we need to handle.
   Some of this is done by the build system normally, so this shouldn't be
   needed when ocamlfdo is called from jenga/dune. *)
let check_artifact file =
  if not (Sys.file_exists_exn file) then (
    if !verbose then
      printf "Missing file after the first phase of compilation: %s\n" file;
    assert false );
  file

let artifacts t kind =
  let rename f = Ocaml_locations.(make_filename kind f) |> check_artifact in
  match t.arts with
  | Stop_before_linear -> assert false
  | Single_src_rename { src = _; target } ->
      (* replace <src>.ml with <target.cmir-linear *)
      [ rename target ]
  | Standard src_files ->
      (* replace <src>.ml with <src.cmir-linear *)
      List.map src_files ~f:rename

type phase =
  | All
  | Compile
  | Emit

(* Set debug "-g" to emit dwarf locations. *)
let phase_flags = function
  | All -> []
  | Compile ->
      if !verbose then printf "stage COMPILE\n";
      [ "-g"; "-stop-after"; "scheduling"; "-save-ir-after"; "scheduling" ]
  | Emit ->
      if !verbose then printf "stage EMIT\n";
      [ "-g"; "-start-from"; "emit" ]

(* correct arguments provided to ocamlopt, depending on the phase. *)
let phase_args t phase =
  let open Ocaml_locations in
  let rename file kind = make_filename kind file |> make_fdo_filename in
  match (t.arts, phase) with
  | Stop_before_linear, _ -> t.args
  | _, All -> t.args
  | Single_src_rename _, Compile -> t.args
  | Standard [], _ -> t.args
  | Standard _, Compile ->
      (* find -o <target> and remove it for the compilation phase, because
         it is incompatible with -c that is implied by -stop-after
         scheduling that we use for split compilation. *)
      remove_targets t.args
  | Single_src_rename { src; target }, Emit ->
      (* replace <src>.ml with <target.cmir-linear-fdo> *)
      List.map t.args ~f:(fun s ->
          if s = src then rename target Linear else s)
  | Standard _, Emit ->
      (* replace <src>.ml with <src.cmir-linear-fdo> *)
      List.map t.args ~f:(fun s ->
          if is_filename Source s then rename s Linear else s)

let call_ocamlopt w phase =
  let args = phase_flags phase @ phase_args w phase in
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
  let args = Option.value args ~default:[] in
  (* collect all args that end in .ml *)
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
            if Ocaml_locations.(is_filename Source s) then Some s else None)
  in
  let arts =
    if stop_before_linear args then Stop_before_linear
    else
      match src_files with
      | [ src ] -> (
          (* If there is only one source file to compile and -o specifies
             the target explicitly, and "-c" or similar specifies
             compilation only mode (i.e., no linking), then the target name
             is used for the names of the intermediate artifacts. *)
          match last_target args with
          | Some target when compilation_only args ->
              Single_src_rename { src; target }
          | _ -> Standard src_files )
      | _ -> Standard src_files
  in
  { args; arts }
