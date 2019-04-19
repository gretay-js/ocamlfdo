open Core

(* Maps functions to layout of the function,
   which is essentially a permutation of original linear ids,
   indexed by the position.
   Spares, i.e., only contains unctions whose layout changed. *)
type layout = (string, (int, int) Hashtbl.t) Hashtbl.t

type reorder_algo =
  | Identity
  | Random
  | External of layout
  | CachePlus

val reorder : reorder_algo -> Cfg.t -> Cfg.t
val validate : reorder_algo -> bool
