(* Manipulate Md5 hashes for intermediate representation per compilation unit
   and function *)
open Core

module Crc : sig
  type t [@@deriving sexp, equal]

  val to_string : t -> string
end

type tbl [@@deriving sexp]
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

val decode_and_add_symbol : t -> string -> unit
(** parses the symbol name and if it consitutes a valid encoding of crc,
    decodes it and adds it to the table. *)

val emit_symbols : t -> Cmm.data_item list

val verbose : bool ref

val merge_into : src:tbl -> dst:tbl -> config -> unit
