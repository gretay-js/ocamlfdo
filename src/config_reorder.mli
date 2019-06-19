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

module Reorder_blocks : sig
  type t =
    | No
    | Opt

  val of_string : string -> t

  val names : string
end
[@@deriving variants]

module Reorder_functions : sig
  type t =
    | No
    | Execounts
    | Hot_clusters

  val of_string : string -> t

  val names : string list
end
[@@deriving variants]

type t = {
  gen_linearid_profile : string;
  write_bolt_fdata : bool;
  write_linker_script : bool;
  reorder_blocks : Reorder_blocks.t;
  reorder_functions : Reorder_functions.t
}

val default : string -> t

val linker_script_filename : t -> string -> string

val bolt_fdata_filename : t -> string -> string

val bolt_decoded_filename : t -> string -> string
