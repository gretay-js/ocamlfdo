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

type reorder_basic_blocks =
  | No
  | Opt

type reorder_functions =
  | No
  | Execounts
  | Hot_clusters

type t = {
  gen_linearid_profile : string;
  write_bolt_fdata : bool;
  write_linker_script : bool;
  reorder_basic_blocks : reorder_basic_blocks;
  reorder_functions : reorder_functions
}

val default : string -> t

val linker_script_filename : t -> string -> string

val bolt_fdata_filename : t -> string -> string

val bolt_decoded_filename : t -> string -> string
