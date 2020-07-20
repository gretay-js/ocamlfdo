open Core

let verbose = ref true

(* CR-someday gyorsh: Improve different option fields. The reason for them is
   that we don't always have all of the information: Loc.t, label, linearid.
   We can have linearid from the cfg without having Loc.t if the the address
   didn't in perf profile. We can reconstruct it but its expensive and we
   currently only use it for debugging, see bolt_profile. Maybe use variant
   to describe the kind of location we have? *)

(* Successor info *)
type b =
  { target : Loc.t option
  ; target_label : Cfg_label.t option
  ; target_id : int option
  ; (* is the target intraprocedural? *)
    intra : bool
  ; fallthrough : bool
  ; (* true for fallthrough targets where counts are inferred from LBR; false
       for branches that appeared explicitly in LBR *)
    mutable taken : Execount.t
  ; mutable mispredicts : Execount.t
  }
[@@deriving sexp, compare]

(* Function Must have at least one of target or target_label *)
(* fallthrough blocks that were inferred from LBR but not directly sampled
   don't have a corresponding raw address. We don't define their target
   location. *)

(* call site info *)
type c =
  { callsite : Loc.t
  ; mutable callees : b list
  }
[@@deriving sexp, compare]

(* Execution counts for a basic block *)
type t =
  { (* Label of this block *)
    label : Cfg_label.t
  ; (* Instruction id for the first and last instruction. *)
    (* [first_id] can be the same as terminator_id if body is empty *)
    first_id : int
  ; terminator_id : int
  ; mutable count : Execount.t
  ; (* Number of times this block was executed. *)
    mutable branches : b list
  ; (* Info about branch targets *)
    mutable calls : c list
  (* Info about call targets *)
  }
[@@deriving sexp]

let mk ~label ~first_id ~terminator_id =
  assert (not (terminator_id = 0));
  assert (not (first_id = 0));
  assert (not (label = 0));
  { label; count = 0L; branches = []; calls = []; first_id; terminator_id }
;;

let add t ~count = t.count <- Execount.(t.count + count)

let find branches branch =
  (* List.partition_tf *)
  List.find branches ~f:(fun b ->
      match b.target, b.target_label, b.target_id with
      | Some t, _, _
        when [%compare.equal: Loc.t option] (Some t) branch.target ->
        assert (
          [%compare.equal: Cfg_label.t option] b.target_label branch.target_label
          && [%compare.equal: int option] b.target_id branch.target_id);
        true
      | None, Some tl, _
        when [%compare.equal: Cfg_label.t option] (Some tl) branch.target_label ->
        assert (
          is_none branch.target
          && [%compare.equal: int option] b.target_id branch.target_id);
        true
      | None, None, Some tid
        when [%compare.equal: Cfg_label.t option] (Some tid) branch.target_id ->
        assert (is_none branch.target && is_none branch.target_label);
        true
      | None, None, None -> assert false
      | _ -> false)
;;

let add_call t ~callsite ~callee =
  (* Find the callsite's info *)
  match List.find t.calls ~f:(fun c -> Loc.equal c.callsite callsite) with
  | None ->
    let c = { callsite; callees = [ callee ] } in
    t.calls <- c :: t.calls
  | Some c ->
    (match
       (* Invariant: unique entry per call target. *)
       (* Find call target entry and update it. *)
       find c.callees callee
     with
    | None -> c.callees <- callee :: c.callees
    | _ -> assert false)
;;

(* Merge maintain unique targets *)
let add_branch t b =
  match find t.branches b with
  | None -> t.branches <- b :: t.branches
  | Some br ->
    (* It must have been one Linear instruction emitted as multiple branch
         instruction to the same target. This should never happen, if the cfg
         is simplified, but now it can happen from Lcondbranch3, which may
         have the same target in 2 of its branches. *)
    assert (Bool.equal b.intra br.intra);
    assert (Bool.equal b.fallthrough br.fallthrough);
    if !verbose
       && not (Execount.equal b.mispredicts 0L && Execount.equal br.mispredicts 0L)
    then (
      printf "Non-zero mispredict value for the same target\n";
      printf !"cur: %{sexp:b}\n" b;
      printf !"new: %{sexp:b}\n" br;
      printf !"all: %{sexp:t}\n" t);
    br.mispredicts <- Execount.(br.mispredicts + b.mispredicts);
    br.taken <- Execount.(br.taken + b.taken)
;;
