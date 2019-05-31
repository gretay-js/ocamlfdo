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
open Base

type 'a interval = {
  l : Int64.t ;
  r : Int64.t ;
  v : 'a
}

type 'a t = 'a interval Map.M(Int64).t

let empty = Map.empty (module Int64)

let enclosing t a =
  match Map.closest_key t `Less_or_equal_to a with
  | None -> None
  | Some (_,interval) ->
    let open Int64 in
    assert (interval.l <= a);
    if a <= interval.r then
      Some interval
    else
      None

(* Checks if t contains k *)
let contains t k =
  match enclosing t k with
  | None -> false
  | Some _ -> true

(* Checks if t has an interval strictly contained in i,
   assuming that t does not contain i.l and i.r themselves. *)
let contained t i =
  match Map.closest_key t `Less_than i.r with
  | None -> false
  | Some (_,interval) ->
    let open Int64 in
    assert (interval.l < i.r);
    assert (interval.r < i.r);
    i.l < interval.l

let insert t interval =
  (* Check that the new interval is disjoint from all existing intervals *)
  (* First, check that the bounds are not contained in another interval *)
  assert (not (contains t interval.l));
  assert (not (contains t interval.r));
  (* Second, check that there is no interval contained between the bounds. *)
  assert (not (contained t interval));
  Map.add_exn t ~key:interval.l ~data:interval
