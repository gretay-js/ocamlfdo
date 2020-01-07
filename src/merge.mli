val verbose : bool ref

val buildid :
  string option -> string option -> ignore_buildid:bool -> string option

module type Profile = sig
  type t

  val read : string -> t

  val write : t -> string -> unit

  val approx_size : t -> int

  val merge_into :
    src:t -> dst:t -> crc_config:Crcs.Config.t -> ignore_buildid:bool -> unit
end

module type Algo = sig
  type profile

  val merge :
    profile ->
    profile ->
    crc_config:Crcs.Config.t ->
    ignore_buildid:bool ->
    profile

  val merge_files :
    string list ->
    crc_config:Crcs.Config.t ->
    ignore_buildid:bool ->
    output_filename:string ->
    unit
end

module Make (P : Profile) : Algo with type profile = P.t
