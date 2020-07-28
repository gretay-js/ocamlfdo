open Core

(* Identifies a block and an instruction in the block. *)
module T = struct
  type t = int * int [@@deriving sexp, compare, equal]
end

include T
module Map = Map.Make(T)
module Set = Set.Make(T)
