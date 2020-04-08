(* Pair of addresses *)
open Core

module T : sig
  type t = Raw_addr.t * Raw_addr.t [@@deriving compare, hash, sexp, bin_io]
end

include module type of T

include module type of Hashable.Make_binable (T)
