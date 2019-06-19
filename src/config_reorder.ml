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

type reorder_blocks =
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
  reorder_blocks : reorder_blocks;
  reorder_functions : reorder_functions
}

let default gen_linearid_profile =
  { gen_linearid_profile;
    write_bolt_fdata = true;
    write_linker_script = true;
    reorder_functions = No;
    reorder_blocks = No
  }

let linker_script_hot_extension = "linker-script-hot"

let bolt_fdata_extension = "fdata"

let linker_script_filename t stage =
  sprintf "%s%s%s.%s" t.gen_linearid_profile
    (if String.is_empty stage then "" else ".")
    stage linker_script_hot_extension

let bolt_fdata_filename t stage =
  sprintf "%s%s%s.%s" t.gen_linearid_profile
    (if String.is_empty stage then "" else ".")
    stage bolt_fdata_extension

let bolt_decoded_filename t stage =
  sprintf "%s%s%s.decoded.%s" t.gen_linearid_profile
    (if String.is_empty stage then "" else ".")
    stage bolt_fdata_extension
