module CL = Ocamlcfg.Cfg_with_layout

type reorder_algo =
  | Identity
  | Random of Core.Random.State.t
  | Profile of Aggregated_decoded_profile.t

val apply : algo:reorder_algo -> CL.t -> alternatives:string list -> CL.t

val verbose : bool ref

val validate : bool ref

val hot_functions :
  Aggregated_decoded_profile.t ->
  reorder_functions:Config_reorder.Reorder_functions.t ->
  cutoff:Config_reorder.Cutoff_functions.t ->
  string list
