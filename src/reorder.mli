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

(* Maps functions to layout of the function,
   which is essentially a permutation of original ids.
   Sparse, i.e., only contains functions whose layout changed. *)
type layout = (int list) String.Map.t

type reorder_algo =
  | Identity
  | Random of Random.State.t
  | Linear of layout
  | Cfg of layout
  | Profile of Profiles.t * Options.t

val reorder
  : algo:reorder_algo
  -> Cfg_builder.t
  -> Cfg_builder.t


val finish : reorder_algo -> unit
