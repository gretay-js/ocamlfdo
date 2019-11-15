open Core

type blocks = Block_info.t Hashtbl.M(Cfg_label).t [@@deriving sexp]
(** Map basic blocks of this function to breakdown of execution counts *)

type t

val create : Ocamlcfg.Cfg_with_layout.t -> Func.t -> t

val blocks : t -> blocks

val record_ip : t -> loc:Loc.t -> data:Execount.t -> unit

val record_trace :
  t -> from_loc:Loc.t -> to_loc:Loc.t -> data:Execount.t -> unit

val record_branch :
  t ->
  from_loc:Loc.t ->
  to_loc:Loc.t ->
  data:Execount.t ->
  mispredicts:Execount.t ->
  unit

val verbose : bool ref
