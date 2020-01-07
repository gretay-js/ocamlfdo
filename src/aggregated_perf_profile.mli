open Core

val verbose : bool ref

(* Pair of addresses *)
module Raw_address_pair : sig
  module T : sig
    type t = Raw_addr.t * Raw_addr.t [@@deriving compare, hash, sexp, bin_io]
  end

  include module type of T

  include module type of Hashable.Make_binable (T)
end

type t =
  { instructions : Execount.t Raw_addr.Table.t;
    branches : Execount.t Raw_address_pair.Table.t;
    mispredicts : Execount.t Raw_address_pair.Table.t;
    traces : Execount.t Raw_address_pair.Table.t;
    mutable buildid : string option
  }
[@@deriving sexp, bin_io]

val empty : unit -> t

val read : string -> t

val write : t -> string -> unit

module Merge : Merge.Algo with type profile = t
