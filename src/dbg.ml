open Core

(* Dwarf info associated with a location *)
type t = {
  file : string;
  line : int;
}
[@@deriving compare, sexp, hash, equal]
