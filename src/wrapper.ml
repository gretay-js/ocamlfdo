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
(* CR-soon gyorsh: Most of this messing with command line arguments wouldn't
   be needed if we could invoke the compiler directly, as a (reentrant)
   library, or as a hook installed into the compiler using pass manager. *)
open Core

let verbose = ref true

type phase =
  | Compile
  | Emit

let call_ocamlopt args phase =
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
