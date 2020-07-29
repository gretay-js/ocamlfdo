open Core

(* Identifies a block and an instruction in the block. *)
type t = int * int [@@deriving sexp, compare, equal]

module Set : Set.S with type Elt.t := t
module Map : Map.S with type Key.t := t
module Map_with_default : Map_with_default.S with type Key.t := t
