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
    | Random
  [@@deriving enumerate]

  val to_string : t -> string

  val default : t
end

module Reorder_functions : sig
  type t =
    | No
    | Execounts
    | Hot_clusters
  [@@deriving enumerate]

  val to_string : t -> string

  val default : t
end

type t = {
  linearid_profile_filename : string;
  write_bolt_fdata : bool;
  write_linker_script : bool;
  linker_script_filename : string option;
  reorder_blocks : Reorder_blocks.t;
  reorder_functions : Reorder_functions.t;
}

val default : string -> t

val get_linker_script_filename : t -> string -> string

val get_bolt_fdata_filename : t -> string -> string

val get_bolt_decoded_filename : t -> string -> string
