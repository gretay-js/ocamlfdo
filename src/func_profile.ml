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
open Loc

type t = {
  id : int;
  (* Unique identifier we assign to this function *)
  name : string;
  (* Name of the function symbol *)
  start : Addr.t;
  (* Raw start address of the function in original binary *)
  mutable count : Execount.t;
  (* Preliminary execution count *)
  mutable has_linearids : bool;
  (* Does the function have any linearids? *)
  agg : Aggregated_perf.t
      (* Counters that refer to this function, uses raw addresses. This can
         be dropped after cfg_count is constructed, to save memory. *)
}
[@@deriving sexp]

let mk ~id ~name ~start =
  { id;
    name;
    start;
    has_linearids = false;
    count = 0L;
    agg = Aggregated_perf.empty ()
  }

(* descending order of execution counts (reverse order of compare) *)
(* Tie breaker using name. Slower than id but more stable w.r.t. changes in
   perf data and ocamlfdo, because ids are an artifact of the way ocamlfdo
   reads and decodes locations. Change to tie breaker using id if speed
   becomes a problem. *)
let compare f1 f2 =
  let res = compare f2.count f1.count in
  if res = 0 then compare f1.name f2.name else res
