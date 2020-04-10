(* Manipulate Md5 hashes for intermediate representation per compilation unit
   and function *)
open Core

module Crc : sig
  type t [@@deriving sexp, equal]
end

type tbl [@@deriving sexp, bin_io]
(** map name to Crc *)

(** What to do with the table *)
type action =
  | Create  (** create a new table *)
  | Compare of tbl  (** compare to existing table *)

exception Near_match of string list

module On_error : sig
  type t =
    | Fail
    | Skip  (** apply profile only to things with matching crcs *)
    | Use_anyway
        (** ignore crc and apply profile whenever name matches. it makes
            sense if the transformation does not use profile, for example,
            random reordering, or if the change in crcs was due to
            compilation in a slightly different environment and not material
            code change. *)
  [@@deriving enumerate, equal]

  val to_string : t -> string

  val default : t
end

module Config : sig
  type t
  (** User can define what crcs to create and compare. *)

  val mk :
    on_mismatch:On_error.t ->
    on_missing:On_error.t ->
    func:bool ->
    unit:bool ->
    ignore_dbg:bool ->
    t
  (** [ignore_dbg] crc saved in the linear file contains source level debug
      info, which will cause a mismatch when the only change is comments or
      formatting. Compute here crc without debug info. This is more
      expensive, but allows us to reuse profiles in more cases. *)

  val report : t -> unit
end

type t
(** Underlying table is mutable *)

val mk : action -> Config.t -> t

val tbl : t -> tbl
(** all we can do to it is save and restore it somewhere *)

val add_unit :
  t -> Linear_format.linear_unit_info -> hex:string -> file:string -> bool

val add_fun : t -> Linear.fundecl -> file:string -> bool

val decode_and_add : t -> string -> unit
(** Parses the symbol name and if it consitutes a valid encoding of crc,
    decodes it and adds it to [t]. *)

val encode : t -> string list
(** Creates symbol names for all the Crcs stored in [t] and clears [t] *)

val verbose : bool ref

val merge_into : src:tbl -> dst:tbl -> Config.t -> unit

val trim : tbl -> Config.t -> unit
(** Trim the profile in place, by keeping only CRCs that are enabled in
    [config]. *)

type stats =
  { func : int;
    unit : int
  }

val get_stats : tbl -> stats
(** number of crcs of each kind in the table *)
