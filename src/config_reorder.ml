open Core

module Reorder_blocks = struct
  type t =
    | No
    | Opt
    | Random
  [@@deriving enumerate]

  let default = No

  let to_string = function
    | No -> "no"
    | Opt -> "opt"
    | Random -> "random"
end

module Reorder_functions = struct
  type t =
    | No
    | Execounts
    | Hot_clusters
    | Random
  [@@deriving enumerate]

  let default = Execounts

  let to_string = function
    | No -> "no"
    | Execounts -> "execution-counts"
    | Hot_clusters -> "hot-clusters"
    | Random -> "random"
end

module Cutoff_functions = struct
  type action =
    | Top_percent of Percent.t
    | Top_percent_samples of Percent.t
    | Top of int
    | Top_clusters of int
    | Min_samples of int
  [@@deriving sexp]

  type t = action list [@@deriving sexp]

  (** keep all functions*)
  let default = [Top_percent (Percent.of_mult 1.)]
end
