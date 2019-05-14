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
(* Control Flow Graph of a function. *)
type t

type label = Linearize.label

module Layout : sig
  type t = label list
end

val from_linear : Linearize.fundecl -> preserve_orig_labels:bool -> t
val to_linear : t -> Linearize.instruction

val get_layout : t -> Layout.t
val set_layout : t -> Layout.t -> t

val get_name : t -> string
val preserve_orig_labels : t -> bool

val id_to_label : t -> int -> label option

(* CR gyorsh: add dot format output *)
val print : out_channel -> t -> unit

(* Mutates t inplace *)
val eliminate_dead_blocks : t -> unit
(* Mutates t inplace and also eliminate dead blocks *)
val eliminate_fallthrough_blocks : t -> unit
