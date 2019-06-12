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
open Core
open Loc

(* Successor info *)
type b = {
  target : Loc.t option;
  target_label : Cfg_label.t option;
  (* is the target intraprocedural? *)
  intra : bool;
  fallthrough : bool;
  (* true for fallthrough targets where counts are inferred from LBR; false
     for branches that appeared explicitly in LBR *)
  taken : Execount.t;
  mispredicts : Execount.t
}
[@@deriving sexp]

(* call site info *)
type c = {
  callsite : Loc.t;
  mutable callees : b list
}
[@@deriving sexp]

(* Execution counts for a basic block *)
type t = {
  label : Cfg_label.t;
  mutable count : Execount.t;
  (* Number of times this block was executed. *)
  mutable branches : b list;
  (* Info about branch targets *)
  mutable calls : c list (* Info about call targets *)
}
[@@deriving sexp]

val mk : label:Cfg_label.t -> t

(* in-place update of mutable fields *)
val add : t -> count:Execount.t -> unit

(* Maintain unique targets *)
val add_call : t -> callsite:c -> callee:b -> unit

val add_branch : t -> b -> unit
