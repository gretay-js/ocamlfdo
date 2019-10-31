open Core

include module type of Int

(* convert from ocamlfdo addr to owee addr *)
val mk : Addr.t -> t
