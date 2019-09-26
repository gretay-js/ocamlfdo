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
type phase =
  | Compile
  | Emit

val call_ocamlopt : string list -> phase option -> unit

val stop_before_linear : string list -> bool

val compilation_only : string list -> bool

val last_target : string list -> string option

val remove_targets : string list -> string list

val check_artifacts : string list -> unit

val verbose : bool ref
