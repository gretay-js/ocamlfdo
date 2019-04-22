open Core

(* Maps functions to layout of the function,
   which is essentially a permutation of original ids,
   indexed by the position.
   Used for raw and relative layout:
   raw layout: linear ids
   relative layout: cfg labels
   Sparse, i.e., only contains functions whose layout changed. *)
type layout = (string, (int, int) Hashtbl.t) Hashtbl.t

type perf_data

type reorder_algo =
  | Identity
  | Random
  | Raw of layout
  | Rel of layout
  | CachePlus of perf_data

val reorder : reorder_algo -> Cfg.t -> gen_rel_layout:string option -> Cfg.t

val validate : reorder_algo -> bool
