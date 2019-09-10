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
(* It should be Cfg.label, but we can't add sexp to Cfg, because the intent
   is to eventually integrate Cfg in the compiler, which doesn't currently
   use sexp. We use sexp to convert to/from file. *)
open Core

module Cfg_label = struct
  type t = int [@@deriving compare, sexp, hash, equal]
end

(* Dwarf info associated with a location *)
type dbg = {
  (* filename *)
  file : string;
  (* line number *)
  line : int;
}
[@@deriving compare, sexp, hash, equal]

type rel = {
  (* Unique id of the containing function symbol *)
  id : int;
  (* Offset from the start of the function *)
  offset : int;
  (* cfg label of the block containing this location *)
  label : Cfg_label.t option;
}
[@@deriving compare, sexp, hash, equal]

type t = {
  addr : Addr.t;
  (* Raw address in the original binary *)
  rel : rel option;
  (* Containing function info and relative offset *)
  dbg : dbg option;
}
[@@deriving sexp, compare, equal]
