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

module Reorder_blocks = struct
  type t =
    | No
    | Opt
    | Random
  [@@deriving enumerate]

  let default = No

  let to_string = function
    | No -> "no"
    | Opt -> "opt"
    | Random -> "random"
end

module Reorder_functions = struct
  type t =
    | No
    | Execounts
    | Hot_clusters
  [@@deriving enumerate]

  let default = No

  let to_string = function
    | No -> "no"
    | Execounts -> "execution-counts"
    | Hot_clusters -> "hot-clusters"
end

type t = {
  linearid_profile_filename : string;
  write_bolt_fdata : bool;
  write_linker_script : bool;
  linker_script_filename : string option;
  reorder_blocks : Reorder_blocks.t;
  reorder_functions : Reorder_functions.t;
}

let default linearid_profile_filename =
  {
    linearid_profile_filename;
    write_bolt_fdata = true;
    write_linker_script = true;
    linker_script_filename = None;
    reorder_functions = Reorder_functions.No;
    reorder_blocks = Reorder_blocks.No;
  }

let linker_script_hot_extension = "linker-script-hot"

let bolt_fdata_extension = "fdata"

let get_linker_script_filename t stage =
  sprintf "%s%s%s.%s" t.linearid_profile_filename
    (if String.is_empty stage then "" else ".")
    stage linker_script_hot_extension

let get_bolt_fdata_filename t stage =
  sprintf "%s%s%s.%s" t.linearid_profile_filename
    (if String.is_empty stage then "" else ".")
    stage bolt_fdata_extension

let get_bolt_decoded_filename t stage =
  sprintf "%s%s%s.decoded.%s" t.linearid_profile_filename
    (if String.is_empty stage then "" else ".")
    stage bolt_fdata_extension
