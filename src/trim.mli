open Core

type action =
  | Top_percent of Percent.t
  | Top_percent_samples of Percent.t
  | Top of int
  | Top_clusters of int
  | Min_samples of int
[@@deriving sexp]

type t = action list [@@deriving sexp]

val apply : t -> (string * Execount.t) list -> (string * Execount.t) list

val of_sexp : Sexp.t -> t
