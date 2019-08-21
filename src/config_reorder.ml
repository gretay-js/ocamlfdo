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
