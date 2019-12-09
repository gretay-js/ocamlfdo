(* Manipulate Md5 hashes for intermediate representation per compilation unit
   and function *)
open Core
module Crc : sig
  type t

  val equals : ?ignore_kind:bool -> t -> t -> bool

  val to_string : t -> string
end

type tbl = private Crc Hashtbl.M(String).t
(** map name to the corresponding md5 *)

type kind =
  | Create
  | Compare of tbl

type t

val mk_tbl : unit -> tbl

val mk : kind -> t

val add_unit : t -> name:string -> Md5.t -> file:string -> unit

val add_fun : t -> Linear.fundecl -> file:string -> unit

val emit_symbols : t -> Cmm.data_item list

val symbol_prefix : string

val symbol_sep : char

val verbose : bool ref
