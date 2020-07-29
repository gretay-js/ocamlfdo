open Core

module T = struct
  type t = int * int [@@deriving sexp, compare, equal]
end

module Map = Map.Make(T)
module Set = Set.Make(T)
module Map_with_default = Map_with_default.Make(T)
include T
