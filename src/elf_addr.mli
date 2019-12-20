open Core

include module type of Int

(* convert from ocamlfdo addr to owee addr *)
val mk : Raw_addr.t -> t

val get : t -> Raw_addr.t
