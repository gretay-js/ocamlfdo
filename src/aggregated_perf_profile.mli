open Core

val verbose : bool ref

type t =
  { instructions : Execount.t Raw_addr.Table.t;
    branches : Execount.t Raw_addr_pair.Table.t;
        (** number of times the branch was taken. *)
    mispredicts : Execount.t Raw_addr_pair.Table.t;
        (** number of times the branch was mispredicted: branch target
            mispredicted or branch direction was mispredicted. *)
    traces : Execount.t Raw_addr_pair.Table.t;
        (** execution count: number of times the trace was taken. *)
    mutable buildid : string option
        (** identifier of the relevant unit (i.e., binary's buildid or
            function's crc in the future), if known. *)
  }
[@@deriving sexp, bin_io]

val empty : unit -> t

val read : string -> t

val write : t -> string -> unit

module Merge : Merge.Algo with type profile = t
