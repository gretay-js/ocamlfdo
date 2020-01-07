type t =
  { id : int;  (** Unique identifier we assign to this function *)
    start : Raw_addr.t;
        (** Raw start address of the function in original binary *)
    finish : Raw_addr.t;
    mutable count : Execount.t;  (** Preliminary execution count *)
    mutable has_linearids : bool;
        (** Does the function have any linearids? *)
    agg : Aggregated_perf_profile.t
        (** Counters that refer to this function, uses raw addresses. *)
  }
(* CR-soon gyorsh: agg can be dropped after cfg_count is constructed, to save
   memory. *)
[@@deriving sexp, bin_io]

val mk : id:int -> start:Raw_addr.t -> finish:Raw_addr.t -> t

val merge : t -> t -> crc_config:Crcs.Config.t -> ignore_buildid:bool -> t
