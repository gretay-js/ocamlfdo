(* Wrap ocamlopt: deals with command line arguments to split compilation
   into two phases, and invokes the compiler. *)

type t

type phase =
  | All
  | Compile
  | Emit

val wrap : string list option -> t

val can_split_compile : t -> bool

val call_ocamlopt : t -> phase -> unit

val artifacts : t -> Ocaml_locations.t -> string list

val verbose : bool ref
