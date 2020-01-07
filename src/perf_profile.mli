val verbose : bool ref

type t

val read_and_aggregate :
  string -> string -> bool -> int list -> Aggregated_perf_profile.t

val read : string -> t
