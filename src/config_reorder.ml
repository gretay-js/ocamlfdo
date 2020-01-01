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
