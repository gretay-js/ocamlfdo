open Core

val verbose : bool ref

type t =
  { instructions : Execount.t Raw_addr.Table.t;
    branches : Execount.t Raw_addr_pair.Table.t;
    mispredicts : Execount.t Raw_addr_pair.Table.t;
    traces : Execount.t Raw_addr_pair.Table.t;
    mutable buildid : string option
  }
[@@deriving sexp, bin_io]

val empty : unit -> t

val read : string -> t

val write : t -> string -> unit

module Merge : Merge.Algo with type profile = t
