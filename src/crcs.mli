(**************************************************************************)
(*                                                                        *)
(*                                 OCamlFDO                               *)
(*                                                                        *)
(*                     Greta Yorsh, Jane Street Europe                    *)
(*                                                                        *)
(*   Copyright 2019 Jane Street Group LLC                                 *)
(*                                                                        *)
(*   All rights reserved.  This file is distributed under the terms of    *)
(*   the GNU Lesser General Public License version 2.1, with the          *)
(*   special exception on linking described in the file LICENSE.          *)
(*                                                                        *)
(**************************************************************************)
(* Manipulate Md5 hashes for intermediate representation per compilation
   unit and function *)
open Core

(* map name to the corresponding md5 *)
type tbl = Md5.t Hashtbl.M(String).t

type kind =
  | Create
  | Compare of tbl

type t

val mk : kind -> t

val add_unit : t -> name:string -> Md5.t -> file:string -> unit

val add_fun : t -> Linear.fundecl -> file:string -> unit

val emit_symbols : t -> Cmm.data_item list

val symbol_prefix : string

val symbol_sep : char
