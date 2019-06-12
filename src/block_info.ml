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

(* Successor info *)
type b = {
  target : Loc.t option;
  target_label : Cfg_label.t option;
  (* is the target intraprocedural? *)
  intra : bool;
  fallthrough : bool;
  (* true for fallthrough targets where counts are inferred from LBR; false
     for branches that appeared explicitly in LBR *)
  taken : Execount.t;
  mispredicts : Execount.t
}
[@@deriving sexp]

(* Function Must have at least one of target or target_label *)
(* fallthrough blocks that were inferred from LBR but not directly sampled
   don't have a corresponding raw address. We don't define their target
   location. *)

(* call site info *)
type c = {
  callsite : Loc.t;
  mutable callees : b list
}
[@@deriving sexp]

(* Execution counts for a basic block *)
type t = {
  label : Cfg_label.t;
  mutable count : Execount.t;
  (* Number of times this block was executed. *)
  mutable branches : b list;
  (* Info about branch targets *)
  mutable calls : c list (* Info about call targets *)
}
[@@deriving sexp]

let mk ~label = { label; count = 0L; branches = []; calls = [] }

let add t ~count = t.count <- Int64.(t.count + count)

let add_call t callsite callee =
  (* Find the callsite's info *)
  match List.find t.calls ~f:(fun c -> c.callsite = callsite) with
  | None ->
      let c = { callsite; callees = [ callee ] } in
      t.calls <- c :: t.calls
  | Some c ->
      (* Check unique call target. *)
      assert (
        Option.is_none
          (List.find c.callees ~f:(fun b -> b.target = callee.target)) );
      c.callees <- callee :: c.callees

(* Merge maintain unique targets *)
let add_branch t b =
  t.branches <- b :: t.branches;
  failwith "not implemented"

(* (\* Find branches target. *\) *)
(* ( match (b.target, b.target_label) with *)
(* | _, Some target_label -> ( *)
(* let existing = *)
(* List.find t.branches ~f:(fun b1 -> *)
(* match b1.target_label with *)
(* | Some lbl when lbl = target_label -> true *)
(* | _ -> false ) *)
(* in *)
(* match existing with *)
(* | Some existing -> *)
(* if !verbose then *)
(* printf *)
(* "Already registered successor target_label %d (existing \ *) (* %d)of
   block at %d\n" *)
(* target_label *)
(* (Option.value existing.target_label ~default:(-17)) *)
(* t.label ; *)
(* assert false *)
(* | _ -> () ) *)
(* | Some target, _ -> *)
(* assert ( *)
(* Option.is_none *)
(* (List.find t.branches ~f:(fun b1 -> *)
(* match b1.target with *)
(* | Some tr when tr = target -> true *)
(* | _ -> false )) ) *)
(* | _ -> assert false ) ; *)
