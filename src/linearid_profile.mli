type t

val mk : unit -> t

val create_cfg_info :
  Aggregated_decoded_profile.t ->
  string ->
  Ocamlcfg.Cfg_with_layout.t ->
  alternatives:string list ->
  Cfg_info.t option

val find : t -> int -> Cfg_info.t option

val add : t -> int -> Cfg_info.t -> unit

val verbose : bool ref
