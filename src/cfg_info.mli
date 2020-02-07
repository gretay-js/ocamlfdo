open Core

type t
(** Map basic blocks of this function to breakdown of execution counts *)

val create : Ocamlcfg.Cfg_with_layout.t -> Func.t -> t

val get_block : t -> Cfg_label.t -> Block_info.t option

val record_ip : t -> loc:Loc.t option -> data:Execount.t -> unit

val record_trace :
  t ->
  from_loc:Loc.t option ->
  to_loc:Loc.t option ->
  data:Execount.t ->
  unit

val record_branch :
  t ->
  from_loc:Loc.t option ->
  to_loc:Loc.t option ->
  data:Execount.t ->
  mispredicts:Execount.t ->
  unit

val verbose : bool ref

val malformed_traces : t -> Execount.t

val dump : t -> unit

val dump_dot : t -> string -> unit

val fold : t -> init:'a -> f:(key:int -> data:Block_info.t -> 'a -> 'a) -> 'a

val iter : t -> f:(Block_info.t -> unit) -> unit
