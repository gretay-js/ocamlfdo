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

(* CR: Improve different option fields. The reason for them is that we don't
   always have all of the information: Loc.t, label, linearid. We can have
   linearid from the cfg without having Loc.t if the the address didn't in
   perf profile. We can reconstruct it but its expensive and we currently
   only use it for debugging, see bolt_profile. Maybe use variant to
   describe the kind of location we have? *)

(* Successor info *)
type b = {
  target : Loc.t option;
  target_label : Cfg_label.t option;
  target_id : int option;
  (* is the target intraprocedural? *)
  intra : bool;
  fallthrough : bool;
  (* true for fallthrough targets where counts are inferred from LBR; false
     for branches that appeared explicitly in LBR *)
  mutable taken : Execount.t;
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
  (* Label of this block *)
  label : Cfg_label.t;
  (* Instruction id for the first and last instruction. *)
  (* [first_id] can be the same as terminator_id if body is empty *)
  first_id : int;
  terminator_id : int;
  mutable count : Execount.t;
  (* Number of times this block was executed. *)
  mutable branches : b list;
  (* Info about branch targets *)
  mutable calls : c list (* Info about call targets *)
}
[@@deriving sexp]

let mk ~label ~first_id ~terminator_id =
  { label; count = 0L; branches = []; calls = []; first_id; terminator_id }

let add t ~count = t.count <- Int64.(t.count + count)

let find branches branch =
  List.find branches ~f:(fun b ->
      match (b.target, b.target_label, b.target_id) with
      | Some t, _, _ when Some t = branch.target ->
          assert (
            b.target_label = branch.target_label
            && b.target_id = branch.target_id );
          true
      | None, Some tl, _ when Some tl = branch.target_label ->
          assert (is_none branch.target && b.target_id = branch.target_id);
          true
      | None, None, Some tid when Some tid = branch.target_id ->
          assert (is_none branch.target && is_none branch.target_label);
          true
      | None, None, None -> assert false
      | _ -> false )

let add_call t ~callsite ~callee =
  (* Find the callsite's info *)
  match List.find t.calls ~f:(fun c -> c.callsite = callsite) with
  | None ->
      let c = { callsite; callees = [ callee ] } in
      t.calls <- c :: t.calls
  | Some c -> (
    (* Invariant: unique entry per call target. *)
    (* Find call target entry and update it. *)
    match find c.callees callee with
    | None -> c.callees <- callee :: c.callees
    | Some _ -> assert false )

(* Merge maintain unique targets *)
let add_branch t b =
  match find t.branches b with
  | None -> t.branches <- b :: t.branches
  | Some br ->
      assert (b.intra && br.intra);
      assert (b.fallthrough && br.fallthrough);
      assert (b.mispredicts = 0L && br.mispredicts = 0L);
      br.taken <- Int64.(br.taken + b.taken)

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
