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

(* Maps functions to layout of the function, which is essentially a
   permutation of original ids. Sparse, i.e., only contains functions whose
   layout changed. *)
type layout = int list String.Map.t

module Config : sig
  type reorder_basic_blocks = No | Opt

  type reorder_functions = No | Execounts | Hot_clusters

  type t =
    { gen_linearid_profile: string
    ; write_bolt_fdata: bool
    ; write_linker_script: bool
    ; reorder_basic_blocks: reorder_basic_blocks
    ; reorder_functions: reorder_functions }

  val default : string -> t

  val linker_script_filename : t -> string -> string
end

type reorder_algo =
  | Identity
  | Random of Random.State.t
  | Linear of layout
  | Cfg of layout
  | Profile of Profiles.Aggregated_decoded.t * Config.t

val reorder : algo:reorder_algo -> Cfg_builder.t -> Cfg_builder.t

val finish : reorder_algo -> unit
