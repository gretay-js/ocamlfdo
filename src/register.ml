open Core

(* Less insane wrapper around the hardware register
 * numbers used by the Linear IR
 *)
module T = struct
  type t = int [@@deriving sexp, compare]
end

include T
module Set = Set.Make(T)
module Map = Map.Make(T)
module Map_with_default = Map_with_default.Make(T)
