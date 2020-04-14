include module type of Core.Int64

val report_overflow : unit -> unit
val ignore_overflow : bool ref

(** detect overflow *)
val ( + ) : t -> t -> t

val add : t -> t -> t
