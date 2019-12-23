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

type config =
  { unit : bool;
    func : bool
  }
(** User can define what crcs to create and compare. *)

type t
(** Underlying table is mutable *)

val mk : action -> config -> t

val tbl : t -> tbl
(** all we can do to it is save and restore it somewhere *)

val add_unit : t -> name:string -> Md5.t -> file:string -> unit

val add_fun : t -> Linear.fundecl -> file:string -> unit

val decode_and_add : t -> string -> unit
(** Parses the symbol name and if it consitutes a valid encoding of crc,
    decodes it and adds it to [t]. *)

val encode : t -> string list
(** Creates symbol names for all the Crcs stored in [t] and clears [t] *)

val verbose : bool ref

val merge_into : src:tbl -> dst:tbl -> config -> unit
