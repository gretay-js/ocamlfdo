(* Pair of addresses *)
open Core

module T = struct
  type t = Raw_addr.t * Raw_addr.t [@@deriving compare, hash, sexp, bin_io]
end

include T
include Hashable.Make_binable (T)
