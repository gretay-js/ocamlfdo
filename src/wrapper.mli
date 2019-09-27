(* Wrap ocamlopt: deals with command line arguments to split compilation
   into two phases, and invokes the compiler. *)

type t

type phase =
  | Compile
  | Emit

val wrap : string list option -> t

val call_ocamlopt : string list -> phase option -> unit

val check_artifacts : string list -> unit

val verbose : bool ref
