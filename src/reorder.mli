open Ocamlcfg

type reorder_algo =
  | Identity
  | Random of Core.Random.State.t
  | Profile of Aggregated_decoded_profile.t

val apply : algo:reorder_algo -> Cfg_builder.t -> Cfg_builder.t

val verbose : bool ref

val validate : bool ref
