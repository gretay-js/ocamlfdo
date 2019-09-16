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

type t = {
  (* Unique identifier we assign to this function *)
  id : int;
  (* Name of the function symbol *)
  name : string;
  (* Raw start address of the function in original binary *)
  start : Addr.t;
  finish : Addr.t;
  (* Preliminary execution count *)
  mutable count : Execount.t;
  (* Does the function have any linearids? *)
  mutable has_linearids : bool;
  (* number of fallthrough traces that didn't correspond to the cfg *)
  mutable malformed_traces : Execount.t;
  (* Counters that refer to this function, uses raw addresses. *)
  (* CR-soon gyorsh: This can be dropped after cfg_count is constructed, to
     save memory. *)
  agg : Aggregated_perf_profile.t;
}
[@@deriving sexp]

let mk ~id ~name ~start ~finish =
  {
    id;
    name;
    start;
    finish;
    has_linearids = false;
    count = 0L;
    malformed_traces = 0L;
    agg = Aggregated_perf_profile.empty ();
  }

(* Descending order of execution counts (reverse order of compare).Tie
   breaker using name. Slower than id but more stable w.r.t. changes in perf
   data and ocamlfdo, because ids are an artifact of the way ocamlfdo reads
   and decodes locations. Change to tie breaker using id if speed becomes a
   problem. *)
let compare f1 f2 =
  let res = Int64.compare f2.count f1.count in
  if res = 0 then String.compare f1.name f2.name else res
