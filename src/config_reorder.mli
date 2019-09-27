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
  [@@deriving enumerate]

  val to_string : t -> string

  val default : t
end
