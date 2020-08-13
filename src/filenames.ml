(* This is a little wrapper around Elf_locations to manage the file names
   specific to ocaml source and compiler's IR. *)
open Core

let verbose = ref true

(* It should be in the pass manager. *)
type t =
  | Source
  | Linear

(* CR-soon gyorsh: what are all the source file extensions we should support? *)
let extension = function
  | Linear -> ".cmir-linear"
  | Source -> ".ml"
;;

let suffix = function
  | Linear -> [ ".cmir-linear" ]
  | Source -> [ ".ml"; ".mli"; ".c"; ".h" ]
;;

let make_fdo file = file ^ "-fdo"

let make_stat file = file ^ "-stat"

(* does s end with one of the extensions of t *)
let is_legal t s = List.exists (suffix t) ~f:(fun suffix -> String.is_suffix s ~suffix)

let make t s =
  match t with
  | Source -> s
  | Linear ->
    let b, _ = Filename.split_extension s in
    b ^ extension t
;;

let compare t ~expected:func ~actual:file =
  match List.find (suffix t) ~f:(fun suffix -> String.is_suffix file ~suffix) with
  | None -> false
  | Some suffix ->
    (match t with
    | Source -> true
    | Linear ->
      (match
         (* Checks that debug info is relative to the input function, i.e.,
             the name of the "file" matches the name of the function. We
             check that the function symbol name from the binary matches the
             function name encoded as filename into our special dwarf info. *)
         String.chop_suffix file ~suffix
       with
      | None -> false
      | Some func_dwarf ->
        if String.equal func_dwarf func
        then true
        else Report.user_error "func_dwarf = %s func = %s\n" func_dwarf func ()))
;;

let to_symbol name =
  let symbol_prefix =
    match X86_proc.system with
    | X86_proc.S_macosx -> "_"
    | _ -> ""
  in
  X86_proc.string_of_symbol symbol_prefix name
;;
