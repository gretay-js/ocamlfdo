module Reorder_blocks : sig
  type t =
    | No
    | Opt
    | Random
  [@@deriving enumerate]

  val to_string : t -> string

  val default : t
end

module Reorder_functions : sig
  type t =
    | No
    | Execounts
    | Hot_clusters
    | Random
  [@@deriving enumerate]

  val to_string : t -> string

  val default : t
end

open Core

module Cutoff_functions : sig
  type action =
    | Top_percent of Percent.t
    | Top_percent_samples of Percent.t
    | Top of int
    | Top_clusters of int
    | Min_samples of int
  [@@deriving sexp]

  type t = action list [@@deriving sexp]

  val default : t
end
