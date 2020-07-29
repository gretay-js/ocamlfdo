open Core

module Cfg = Ocamlcfg.Cfg
module BB = Cfg.Basic_block

module T = struct
  type t = int * int [@@deriving sexp, compare, equal]
end

include T
module Set = Set.Make(T)
module Map = Map.Make(T)
module Map_with_default = Map_with_default.Make(T)
