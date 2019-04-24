open Core

(* Maps functions to layout of the function,
   which is essentially a permutation of original ids.
   Sparse, i.e., only contains functions whose layout changed. *)
type layout = (string, int list) Hashtbl.t

type reorder_algo =
  | Identity
  | Random
  | Linear_id of layout
  | Cfg_label of layout
  | CachePlus of unit

val reorder
  : algo:reorder_algo
  -> Cfg.t
  -> write_rel_layout:(string -> int list -> unit)
  -> Cfg.t

val validate : reorder_algo -> bool
