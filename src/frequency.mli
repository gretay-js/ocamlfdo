include module type of Execount

val create : Cfg_info.t option -> Ocamlcfg.Label.t -> t

val lub : t -> t -> t
