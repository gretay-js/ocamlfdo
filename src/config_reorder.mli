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
