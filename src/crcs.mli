(* Manipulate Md5 hashes for intermediate representation per compilation unit
   and function *)
open Core

module Crc : sig
  type t [@@deriving sexp]

  val equal : ?ignore_kind:bool -> t -> t -> bool

  val to_string : t -> string
end

type action =
  | Create
  | Compare of tbl

type t

val mk_tbl : unit -> tbl

val mk : action -> t

val add_unit : t -> name:string -> Md5.t -> file:string -> unit

val add_fun : t -> Linear.fundecl -> file:string -> unit

val decode_symbol : t -> string -> unit
(** parses the symbol name and if it consitutes a valid encoding of crc,
    decodes it and adds it to the table. *)

val emit_symbols : t -> Cmm.data_item list

val verbose : bool ref
