open Core

module type Key = Map.Key

module type S = sig
  module Key : Key

  type +'a t [@@deriving sexp]

  val default : 'a -> 'a t

  val find : 'a t -> Key.t -> 'a

  val update : 'a t -> Key.t -> f:('a -> 'a) -> 'a t

  val set : 'a t -> key:Key.t -> data:'a -> 'a t

  val merge
    :  'a t
    -> 'b t
    -> f:('a -> 'b -> 'c)
    -> 'c t

  val map
    :  'a t
    -> f:('a -> 'b)
    -> 'b t

  val equal : ('a -> 'a -> bool) -> 'a t -> 'a t -> bool
end
