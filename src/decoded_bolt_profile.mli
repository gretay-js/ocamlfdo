type t

val create : Elf_locations.t -> filename:string -> t

val export : t -> Aggregated_perf_profile.t

val save :
  Aggregated_decoded_profile.t ->
  Aggregated_perf_profile.t ->
  filename:string ->
  unit

val save_fallthrough :
  Aggregated_decoded_profile.t -> filename:string -> unit

val write : t -> filename:string -> unit

val verbose : bool ref
