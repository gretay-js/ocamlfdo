open Core

(* Wrapper around integers and classes which identifies a spill slot *)
type t = int * int [@@deriving sexp, compare]

module Set : Set.S with type Elt.t := t
module Map : Map.S with type Key.t := t

val all_spills : Ocamlcfg.Cfg.t -> Set.t

val all_reloads : Ocamlcfg.Cfg.t -> t -> Inst_id.Set.t

