val percent : int -> int -> float

(** Some function names come out too long to use as filenames. Shorten them. *)
val get_filename : name:string -> title:string -> sub:string -> string

val logf : ('a, Format.formatter, unit, unit, unit, unit) Core.format6 -> 'a
val start : unit -> unit
val finish : unit -> unit

module Hint : sig
  type t =
    | Old_profile
    | Mismatch

  val to_fmt : t -> ('a, 'b, 'c, 'd, 'd, 'a) Core.format6
end

val user_error
  :  ?hint:Hint.t option
  -> ?exn:exn option
  -> ('a, Format.formatter, unit, unit, unit, 'b) Core.format6
  -> 'a

val verbose : bool ref
