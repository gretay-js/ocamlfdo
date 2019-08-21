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
open Ocamlcfg

type reorder_algo =
  | Identity
  | Random of Core.Random.State.t
  | Profile of Aggregated_decoded_profile.t

val apply : algo:reorder_algo -> Cfg_builder.t -> Cfg_builder.t

val verbose : bool ref

val validate : bool ref
