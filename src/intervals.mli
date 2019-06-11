(**************************************************************************)
(*                                                                        *)
(*                                 OCamlFDO                               *)
(*                                                                        *)
(*                     Greta Yorsh, Jane Street Europe                    *)
(*                                                                        *)
(*   Copyright 2019 Jane Street Group LLC                                 *)
(*                                                                        *)
(*                     based on spacetime_lib                             *)
(*   Copyright (c) 2016 Leo White, Mark Shinwell                          *)
(*   https://github.com/lpw25/spacetime_lib                               *)
(*                                                                        *)
(*   All rights reserved.  This file is distributed under the terms of    *)
(*   the GNU Lesser General Public License version 2.1, with the          *)
(*   special exception on linking described in the file LICENSE.          *)
(*                                                                        *)
(**************************************************************************)
(* Intervals data structure that stores disjoint closed intervals. Supports
   efficient add and find (assuming that map's find is efficient). We don't
   need an interval tree, because all intervals are disjoint. *)
type 'a interval = {l: Int64.t; r: Int64.t; v: 'a}

type 'a t

val empty : 'a t

val insert : 'a t -> 'a interval -> 'a t

val enclosing : 'a t -> Int64.t -> 'a interval option
