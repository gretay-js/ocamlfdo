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
[@@@ocaml.warning "+a-4-30-40-41-42-44-45"]
open Linearize
open Cfg

type label = Linearize.label

module Layout = struct
  type t = label list
end

let verbose = false

let successors block =
  match block.terminator.desc with
  | Branch successors -> successors
  | Return -> []
  | Raise _ -> []
  | Tailcall _ -> []
  | Switch labels ->
    Array.mapi (fun i label ->
      (Test(Iinttest_imm(Iunsigned Ceq, i)), label))
      labels
    |> Array.to_list

let successor_labels block =
  let (_, labels) = List.split (successors block) in
  labels

type t = {
  cfg : Cfg.t;
  mutable layout : Layout.t;      (* Original layout: linear order of blocks. *)
  mutable new_labels : LabelSet.t;
  (* Labels added by cfg construction, except entry. Used for split_labels. *)

  split_labels : (label, Layout.t) Hashtbl.t;
  (* Maps original label [L] to the sequence of labels of blocks
     that represent block [L] in the cfg. Sparse: only contains information
     for blocks that were split or eliminated during cfg construction.
     Used for mapping information about original blocks,
     such as perf annotations, exection counts or new layout, to cfg blocks. *)

  trap_depths : (label, int) Hashtbl.t;
  (* Map labels to trap depths for linearize. *)

  trap_labels : (label, label) Hashtbl.t;
  (* Maps trap handler block label [L] to the label of the block where the
     Lpushtrap L reference it. Used for dead block elimination.
     This mapping is one to one, but the reverse is not, because
     a block might contain multiple Lpushtrap, which is not a terminator. *)

  mutable id_to_label : label Numbers.Int.Map.t;
  (* Map id of instruction to label of the block that contains
     the instruction. Used for mapping perf data back to linear IR. *)

  mutable preserve_orig_labels : bool;
  (* Unset to false for validation, set for optimization. *)
}

let get_layout t = t.layout
let set_layout t new_layout =
  t.layout <- new_layout;
  t

let get_name t = t.cfg.fun_name

let preserve_orig_labels t = t.preserve_orig_labels

let no_label = (-1)
type labelled_insn =
  { label : label;
    insn : Linearize.instruction;
  }

let create_empty_instruction ?(trap_depth=0) desc =
  { desc;
    arg = [||]; res = [||]; dbg = Debuginfo.none; live = Reg.Set.empty;
    trap_depth;
    id = 0;
  }

let create_empty_block t start =
  let block = { start;
                body = [];
                terminator = create_empty_instruction (Branch []);
                predecessors = LabelSet.empty;
              } in
  if Hashtbl.mem t.cfg.blocks start then
    Misc.fatal_error("Cannot create block, label exists: " ^
                     (string_of_int start));
  t.layout <- start::t.layout;
  block

let register t block =
  if Hashtbl.mem t.cfg.blocks block.start then
    Misc.fatal_error("Cannot register block, label exists: "
                     ^ (string_of_int block.start));
  (* Printf.printf "registering block %d\n" block.start *)
  (* Body is constructed in reverse, fix it now: *)
  block.body <- List.rev block.body;
  Hashtbl.add t.cfg.blocks block.start block

let register_predecessors t =
  Hashtbl.iter (fun label block ->
    let targets = successor_labels block in
    List.iter (fun target ->
      let target_block = Hashtbl.find t.cfg.blocks target in
      (* Add label to predecessors of target  *)
      target_block.predecessors <-
        LabelSet.add label target_block.predecessors)
      targets
  ) t.cfg.blocks

let is_live_trap_handler t label =
  Hashtbl.mem t.trap_labels label

let register_split_labels t =
  List.fold_right (fun label new_labels_layout ->
    if (LabelSet.mem label t.new_labels) then
      (* Add a new label to accumulated layout *)
      label::new_labels_layout
    else begin (* Original label found *)
      if new_labels_layout <> [] then begin
        (* The original label was followed by some new ones,
           which we have gathers in new_labels_layout.
           Tuck on original label and register the split layout. *)
        Hashtbl.add t.split_labels label (label::new_labels_layout);
      end;
      []
    end)
    t.layout
    []
  |> ignore

let create_instr desc ~trap_depth (i:Linearize.instruction) =
  {
    desc = desc;
    arg = i.arg;
    res = i.res;
    dbg = i.dbg;
    live = i.live;
    trap_depth;
    id = i.id;
  }

let get_or_make_label t (i : Linearize.instruction) =
  match i.desc with
  | Llabel label -> { label; insn = i; }
  (* | Lbranch _ | Lcondbranch (_,_) | Lcondbranch3(_,_,_)
   *   -> Misc.fatal_errorf "Unexpected branch instead of label @;%a"
   *                  Printlinear.instr i; *)
  | Lend -> Misc.fatal_errorf "Unexpected end of function instead of label@;%a"
      Printlinear.instr i
  | _ -> let label = Cmm.new_label () in
    t.new_labels <- LabelSet.add label t.new_labels;
    { label;
      insn = Linearize.instr_cons (Llabel label) [||] [||] i;
    }

(* Is [i] an existing label? *)
let rec has_label (i : Linearize.instruction) =
  begin match i.desc with
  | Lend | Llabel _ -> true
  | Ladjust_trap_depth _ -> has_label i.next
  | _ ->
    Misc.fatal_errorf
      "Unexpected instruction after terminator @;%a"
      Printlinear.instr i
  end

let mark_trap_label t ~lbl_handler ~lbl_pushtrap_block =
  if (Hashtbl.mem t.trap_labels lbl_handler) then
    Misc.fatal_errorf "Trap hanlder label already exists: \
                       Lpushtrap %d from block label %d\n"
      lbl_handler
      lbl_pushtrap_block;
  Hashtbl.add t.trap_labels lbl_handler lbl_pushtrap_block

let from_basic = function
  | Reloadretaddr -> Lreloadretaddr
  | Entertrap -> Lentertrap
  | Pushtrap { lbl_handler } -> Lpushtrap { lbl_handler }
  | Poptrap -> Lpoptrap
  | Call(F(Indirect {label_after})) -> Lop(Icall_ind {label_after})
  | Call(F(Immediate {func;label_after})) -> Lop(Icall_imm {func; label_after})
  | Call(P(External {func; alloc; label_after})) ->
    Lop(Iextcall {func; alloc; label_after})
  | Call(P(Checkbound({immediate=None; label_after_error; spacetime_index}))) ->
    Lop(Iintop(Icheckbound {label_after_error; spacetime_index}))
  | Call(P(Checkbound({immediate = Some i;
                       label_after_error;
                       spacetime_index}))) ->
    Lop(Iintop_imm(Icheckbound {label_after_error; spacetime_index},i))
  | Call(P(Alloc {words;label_after_call_gc;spacetime_index})) ->
    Lop(Ialloc {words;label_after_call_gc;spacetime_index})
  | Op(op) ->
    match op with
    | Move -> Lop(Imove)
    | Spill -> Lop(Ispill)
    | Reload -> Lop(Ireload)
    | Const_int n -> Lop(Iconst_int n)
    | Const_float n -> Lop(Iconst_float n)
    | Const_symbol n -> Lop(Iconst_symbol n)
    | Stackoffset n -> Lop(Istackoffset n)
    | Load(c,m) -> Lop(Iload(c,m))
    | Store(c,m,b) -> Lop(Istore(c,m,b))
    | Intop op -> Lop(Iintop op)
    | Intop_imm(op, i) -> Lop(Iintop_imm(op, i))
    | Negf -> Lop(Inegf)
    | Absf -> Lop(Iabsf)
    | Addf -> Lop(Iaddf)
    | Subf -> Lop(Isubf)
    | Mulf -> Lop(Imulf)
    | Divf -> Lop(Idivf)
    | Floatofint -> Lop(Ifloatofint)
    | Intoffloat -> Lop(Iintoffloat)
    | Specific op -> Lop(Ispecific op)
    | Name_for_debugger {ident;which_parameter;provenance;is_assignment;} ->
      Lop(Iname_for_debugger {ident;which_parameter;provenance;is_assignment;})

let record_trap_depth_at_label t label ~trap_depth =
  match Hashtbl.find t.trap_depths label with
  | exception Not_found ->
    Hashtbl.add t.trap_depths label trap_depth
  | existing_trap_depth ->
    if trap_depth <> existing_trap_depth then
      Misc.fatal_errorf "Conflicting trap depths for label %d: already have \
          %d but the following instruction has depth %d"
        label
        existing_trap_depth
        trap_depth


let rec create_blocks t (i : Linearize.instruction) block ~trap_depth =
  let add_terminator desc =
    block.terminator <- create_instr desc ~trap_depth i;
    register t block
  in
    match i.desc with
    | Lend ->
      (* End of the function. Make sure the previous block is registered. *)
      if not (Hashtbl.mem t.cfg.blocks block.start) then
        Misc.fatal_errorf
          "End of function without terminator for block %d\n"
          block.start
    | Llabel start ->
      (* Add the previos block, if it did not have an explicit terminator. *)
      if not (Hashtbl.mem t.cfg.blocks block.start) then begin
        (* Previous block falls through. Add start as explicit successor. *)
        let fallthrough = Branch [(Always,start)] in
        block.terminator <- create_empty_instruction fallthrough ~trap_depth;
        register t block
      end;
      (* Start a new block *)
      (* CR gyorsh: check for multpile consecutive labels *)
      record_trap_depth_at_label t start ~trap_depth;
      let new_block = create_empty_block t start in
      create_blocks t i.next new_block ~trap_depth

    | Lop(Itailcall_ind {label_after}) ->
      let desc = Tailcall(Indirect {label_after}) in
      assert (has_label i.next);
      add_terminator desc;
      create_blocks t i.next block ~trap_depth

    | Lop(Itailcall_imm {func; label_after}) ->
      let desc = Tailcall(Immediate{func;label_after}) in
      assert (has_label i.next);
      add_terminator desc;
      create_blocks t i.next block ~trap_depth

    | Lreturn ->
      assert (has_label i.next);
      if trap_depth <> 0 then
        Misc.fatal_error "Trap depth must be zero at Lreturn";
      add_terminator Return;
      create_blocks t i.next block ~trap_depth

    | Lraise(kind) ->
      assert (has_label i.next);
      add_terminator (Raise kind);
      create_blocks t i.next block ~trap_depth

    | Lbranch lbl ->
      let successors = [(Always,lbl)] in
      assert (has_label i.next);
      record_trap_depth_at_label t lbl ~trap_depth;
      add_terminator (Branch successors);
      create_blocks t i.next block ~trap_depth

    | Lcondbranch(cond,lbl) ->
      (* CR gyorsh: merge (Lbranch | Lcondbranch | Lcondbranch3)+
         into a single terminator when the argments are the same.
         Enables reordering of branch instructions and save cmp instructions.
         The main problem is that it involves
         boolean combination of conditionals of type Mach.test
         that can arise from a sequence of branches.
         When all conditions in the combination are integer comparisons,
         we can simplify them into a single condition, but it doesn't work for
         Ieventest and Ioddtest (which come from the primitive "is integer").
         The advantage is that it will enable us to reorder branch
         instructions to avoid generating jmp to fallthrough location
         in the new order.
         Also, for linear to cfg and back will be
         harder to generate exactly the same layout.
         Also, how do we map execution counts about branches
         onto this terminator? *)
      let fallthrough = get_or_make_label t i.next in
      let successors = [(Test cond,lbl);
                        (Test (invert_test cond),fallthrough.label)] in
      add_terminator (Branch successors);
      record_trap_depth_at_label t lbl ~trap_depth;
      record_trap_depth_at_label t fallthrough.label ~trap_depth;
      create_blocks t fallthrough.insn block ~trap_depth

    | Lcondbranch3(lbl0,lbl1,lbl2) ->
      let fallthrough = get_or_make_label t i.next in
      let get_dest lbl =
        let res = match lbl with
          | None -> fallthrough.label
          | Some lbl -> lbl
            ;
        in
        record_trap_depth_at_label t res ~trap_depth;
        res
      in
      let s0 = (Test(Iinttest_imm(Iunsigned Clt, 1)), get_dest lbl0) in
      let s1 = (Test(Iinttest_imm(Iunsigned Ceq, 1)), get_dest lbl1) in
      let s2 = (Test(Iinttest_imm(Isigned   Cgt, 1)), get_dest lbl2) in
      add_terminator (Branch [s0;s1;s2]);
      create_blocks t fallthrough.insn block ~trap_depth

    | Lswitch labels ->
      (* CR gyorsh: get rid of switches entirely
         and re-generate them based on optimization and perf data *)
      add_terminator (Switch labels);
      Array.iter (record_trap_depth_at_label t ~trap_depth) labels;
      assert (has_label i.next);
      create_blocks t i.next block ~trap_depth

    | Ladjust_trap_depth { delta_traps } ->
      (* We do not emit any executable code for this insn, only moves
         the virtual stack pointer.
         We do not have an insn in cfg because the required adjustment
         can change when blocks are reordered,
         regenerate it when converting back to linear.
         We use delta_traps only to compute trap_depths of other instructions.*)
      let trap_depth = trap_depth + delta_traps in
      if trap_depth < 0 then
        Misc.fatal_errorf "Ladjust_trap_depth %d moves the trap depth \
                           below zero: %d"
          delta_traps trap_depth;
      create_blocks t i.next block ~trap_depth

    | Lpushtrap { lbl_handler } ->
      mark_trap_label t ~lbl_handler ~lbl_pushtrap_block:block.start;
      record_trap_depth_at_label t lbl_handler ~trap_depth;
      let desc = Pushtrap { lbl_handler } in
      block.body <- (create_instr desc ~trap_depth i)::block.body;
      let trap_depth = trap_depth + 1 in
      create_blocks t i.next block ~trap_depth

    | Lpoptrap ->
      let desc = Poptrap in
      block.body <- (create_instr desc ~trap_depth i)::block.body;
      let trap_depth = trap_depth - 1 in
      if trap_depth < 0 then
        Misc.fatal_error "Lpoptrap moves the trap depth below zero";
      create_blocks t i.next block ~trap_depth

    | d ->
      let desc = begin match d with
        | Lentertrap -> Entertrap
        | Lreloadretaddr -> Reloadretaddr
        | Lop(op) -> begin match op with
          | Icall_ind { label_after} -> Call(F(Indirect {label_after}))
          | Icall_imm {func; label_after} ->
            Call(F(Immediate {func;label_after}))
          | Iextcall {func; alloc; label_after} ->
            Call(P(External {func; alloc; label_after}))
          | Iintop(op) -> begin match op with
            | Icheckbound {label_after_error; spacetime_index} ->
              Call(P(Checkbound{ immediate = None;
                                 label_after_error;
                                 spacetime_index}))
            | _ -> Op(Intop(op))
          end
          | Iintop_imm(op,i) -> begin match op with
            | Icheckbound {label_after_error; spacetime_index} ->
              Call(P(Checkbound ({immediate = Some i;
                                label_after_error;
                                spacetime_index; })))
            |_ -> Op(Intop_imm(op, i))
          end
          | Ialloc {words;label_after_call_gc;spacetime_index} ->
            Call(P(Alloc {words;label_after_call_gc;spacetime_index}))
          | Istackoffset i -> Op(Stackoffset i)
          | Iload(c,a) -> Op(Load(c,a))
          | Istore(c,a,b)-> Op(Store(c,a,b))
          | Imove -> Op Move
          | Ispill -> Op Spill
          | Ireload -> Op Reload
          | Iconst_int n -> Op(Const_int n)
          | Iconst_float n -> Op(Const_float n)
          | Iconst_symbol n -> Op(Const_symbol n)
          | Inegf -> Op Negf
          | Iabsf -> Op Absf
          | Iaddf -> Op Addf
          | Isubf -> Op Subf
          | Imulf -> Op Mulf
          | Idivf -> Op Divf
          | Ifloatofint -> Op Floatofint
          | Iintoffloat -> Op Intoffloat
          | Ispecific op -> Op(Specific op)
          | Iname_for_debugger {ident;
                                which_parameter;
                                provenance;
                                is_assignment;} ->
            Op(Name_for_debugger {ident;
                                  which_parameter;
                                  provenance;
                                  is_assignment;})
          | Itailcall_ind _ | Itailcall_imm _ -> assert (false)
        end
        | Lend| Lreturn| Llabel _
        | Lbranch _| Lcondbranch (_, _)| Lcondbranch3 (_, _, _)
        | Lswitch _| Lraise _ | Ladjust_trap_depth _
        | Lpoptrap|Lpushtrap _-> assert (false)
      end
      in
      block.body <- (create_instr desc i ~trap_depth)::block.body;
      create_blocks t i.next block ~trap_depth

let make_empty_cfg name ~preserve_orig_labels =
  let cfg =
  {
    fun_name = name;
    entry_label = 0;
    blocks = (Hashtbl.create 31 : (label, block) Hashtbl.t);
  }
  in
  {
    cfg;
    trap_labels = (Hashtbl.create 7 : (label, label) Hashtbl.t);
    trap_depths = (Hashtbl.create 31 : (label, int) Hashtbl.t);
    new_labels = LabelSet.empty;
    split_labels = (Hashtbl.create 7 : (label, Layout.t) Hashtbl.t);
    layout = [];
    id_to_label = Numbers.Int.Map.empty;
    preserve_orig_labels;
  }

let compute_id_to_label t =
  let fold_block map label =
    let block = Hashtbl.find t.cfg.blocks label in
    let new_map = List.fold_left (fun map i->
      Numbers.Int.Map.add i.id label map)
      map
      block.body
    in Numbers.Int.Map.add block.terminator.id label new_map
  in
  t.id_to_label <- List.fold_left fold_block
                     Numbers.Int.Map.empty
                     t.layout

let from_linear (f : Linearize.fundecl) ~preserve_orig_labels =
  let t = make_empty_cfg f.fun_name ~preserve_orig_labels in
  (* CR gyorsh: label of the function entry must not conflict with existing
     labels. Relies on the invariant: Cmm.new_label() is int > 99.
     An alternative is to create a new type for label here,
     but it is less efficient because label is used as a key to Hashtble. *)
  let entry_block = create_empty_block t t.cfg.entry_label in
  create_blocks t f.fun_body entry_block ~trap_depth:0;
  (* Register predecessors now rather than during cfg construction,
     because of forward jumps: the blocks do not exist when the jump
     that reference them is processed.
     CR gyorsh: combine with dead block elimination. *)
  register_predecessors t;
  register_split_labels t;
  compute_id_to_label t;
  (* Layout was constructed in reverse, fix it now: *)
  t.layout <- List.rev t.layout;
  t

(* Set desc and next from inputs and the rest is empty *)
let make_simple_linear desc next =
  { desc; next;
    arg = [||]; res = [||]; dbg = Debuginfo.none; live = Reg.Set.empty;
    id = 0;
  }

(* Set desc and next from inputs and copy the rest from i *)
let to_linear_instr ~i desc next =
  { desc; next;
    arg = i.arg; res = i.res; dbg = i.dbg; live = i.live;
    id = i.id
  }

let basic_to_linear i next =
  let desc = from_basic i.desc in
  to_linear_instr desc next ~i

let linearize_terminator terminator next =
  let desc_list =
    match terminator.desc with
    | Return -> [Lreturn]
    | Raise kind -> [Lraise kind]
    | Tailcall(Indirect {label_after}) ->
      [Lop(Itailcall_ind {label_after})]
    | Tailcall(Immediate {func;label_after}) ->
      [Lop(Itailcall_imm {func;label_after})]
    | Switch labels -> [Lswitch labels]
    | Branch successors ->
      match successors with
      | [] ->
        if verbose then Printf.printf "next label is %d\n" next.label;
        Misc.fatal_error "Branch without successors"
      | [(Always,label)] ->
        if next.label = label then []
        else [Lbranch(label)]
      | [(Test _, _)] -> Misc.fatal_error ("Successors not exhastive");
      | [(Test cond_p,label_p); (Test cond_q,label_q)] ->
        if cond_p <> invert_test cond_q then
          Misc.fatal_error ("Illegal successors")
          (* [Lcondbranch(cond_p,label_p); Lcondbranch(cond_q,label_q); ] *)
        else if label_p = next.label && label_q = next.label then
          []
        else if label_p <> next.label && label_q <> next.label then
          (* CR gyorsh: if both label are not fall through, then arrangement
             should depend on perf data and possibly
             the relative position of the target labels
             and the current block: whether the jumps are forward or back.
             This information can be obtained from layout but it needs
             to be made accessible here.
          *)
          [Lcondbranch(cond_p,label_p); Lbranch(label_q)]
        else if label_p = next.label then
          [Lcondbranch(cond_q,label_q)]
        else if label_q = next.label then
          [Lcondbranch(cond_p,label_p)]
        else assert false
      | [(Test(Iinttest_imm(Iunsigned Clt, 1)),label0);
         (Test(Iinttest_imm(Iunsigned Ceq, 1)),label1);
         (Test(Iinttest_imm(Isigned   Cgt, 1)),label2)] ->
        let find_label l =
          if next.label = l then None
          else Some l
        in
        [Lcondbranch3(find_label label0,
                      find_label label1,
                      find_label label2)]
      | _ -> assert (false)
  in
  List.fold_right (to_linear_instr ~i:terminator) desc_list next.insn

(* CR gyorsh: handle duplicate labels in new layout: print the same
   block more than once. *)
let to_linear t =
  let layout = Array.of_list t.layout in
  let len = Array.length layout in
  let next = ref { label = no_label; insn = end_instr; } in
  for i = len - 1 downto 0 do
    let label = layout.(i) in
    if not (Hashtbl.mem t.cfg.blocks label) then
      Misc.fatal_errorf "Unknown block labelled %d\n" label;
    let block = Hashtbl.find t.cfg.blocks label in
    let terminator = linearize_terminator block.terminator !next  in
    let body = List.fold_right basic_to_linear block.body terminator in
    let insn =
      if i = 0 then begin (* First block, don't add label. *)
        body
      end
      else begin
        let pred = layout.(i-1) in
        let body =
          if block.predecessors = LabelSet.singleton pred then begin
            if is_live_trap_handler t block.start then
              Misc.fatal_errorf "Fallthrough from %d to trap handler %d\n"
                pred block.start;
            (* Single predecessor is immediately prior to this block,
               no need for the label. *)
            (* CR gyorsh: is this correct with label_after for calls? *)
            (* If label is original,
               print it for linear to cfg and back to be identity,
               and for various assertions in reorder. *)
            if LabelSet.mem label t.new_labels ||
               not t.preserve_orig_labels then begin
              body
            end
            else
              make_simple_linear (Llabel label) body
          end
          else begin
            make_simple_linear (Llabel label) body
          end
        in
        (* Adjust trap depth *)
        let pred_block = Hashtbl.find t.cfg.blocks pred in
        let block_trap_depth = Hashtbl.find t.trap_depths label in
        let pred_trap_depth = pred_block.terminator.trap_depth in
        if block_trap_depth != pred_trap_depth then
          let delta_traps = block_trap_depth - pred_trap_depth in
          make_simple_linear (Ladjust_trap_depth { delta_traps }) body
        else body
      end in
    next := { label; insn; }
  done;
  !next.insn

let print oc t =
  let ppf = Format.formatter_of_out_channel oc in
  let print_instr i =
    Printlinear.instr ppf (basic_to_linear i end_instr)
  in
  let print_terminator ti =
    Printlinear.instr ppf (linearize_terminator ti
                             { label = no_label;
                               insn = end_instr;
                             }) in
  Printf.fprintf oc "%s\n" t.cfg.fun_name;
  Printf.fprintf oc "layout.length=%d\n" (List.length t.layout);
  Printf.fprintf oc "blocks.length=%d\n" (Hashtbl.length t.cfg.blocks);
  let print_block label block =
      Printf.fprintf oc "\n%d:\n" label;
      List.iter (fun i->
        Printf.fprintf oc "\n\t";
        print_instr i)
        block.body;
      (* Printf.fprintf oc "\t%d\n" block.terminator.id; *)
      print_terminator block.terminator;
      Printf.fprintf oc "\npredecessors:";
      LabelSet.iter (fun l -> Printf.fprintf oc " %d" l)
        block.predecessors;
      Printf.fprintf oc "\nsuccessors:";
      List.iter (fun l -> Printf.fprintf oc " %d" l)
        (successor_labels block)
  in
  List.iter (fun label ->
    let block = Hashtbl.find t.cfg.blocks label
    in print_block label block)
    t.layout;
  Printf.fprintf oc "id_to_label map: \n";
  Numbers.Int.Map.iter (fun id lbl -> Printf.printf "(%d,%d) "  id lbl)
    t.id_to_label;
  Printf.fprintf oc "\n"

let id_to_label t id =
  match Numbers.Int.Map.find_opt id t.id_to_label with
  | None ->
    print stdout t;
    Misc.fatal_errorf "Cannot find label for id %d in map\n" id
  | Some lbl ->
    if verbose then
      Printf.printf "Found label %d for id %d in map\n" lbl id;
    Some lbl

(* Simplify CFG *)
(* CR gyorsh: needs more testing. *)

(* CR gyorsh: eliminate transitively blocks that become dead from this one. *)
let eliminate_dead_block t dead_blocks label =
  let block = Hashtbl.find t.cfg.blocks label in
  Hashtbl.remove t.cfg.blocks label;
  (* Update successor blocks of the dead block *)
  List.iter (fun target ->
    let target_block = Hashtbl.find t.cfg.blocks target in
    (* Remove label from predecessors of target. *)
    target_block.predecessors <- LabelSet.remove
                                   label
                                   target_block.predecessors)
    (successor_labels block);
  (* Remove from layout and other data-structures that track labels. *)
  t.layout <- List.filter (fun l -> l <> label) t.layout;
  (* If the dead block contains Lpushtrap, its handler becomes dead.
     Find all occurrences of label as values of trap_labels
     and remove them, because is_live_trap_handler depends on it. *)
  Hashtbl.filter_map_inplace
    (fun _ lbl_pushtrap_block ->
       if label = lbl_pushtrap_block then None
       else Some lbl_pushtrap_block)
    t.trap_labels;

  (* If dead block's label is not new, add it to split_labels,
     mapped to the empty layout!
     Needed for mapping annotations that may refer to dead blocks. *)
  if not (LabelSet.mem label t.new_labels) then
    Hashtbl.add t.split_labels label [];

  (* Not necessary to remove it from trap_depths, because it will
     only be accessed if found in the cfg, but remove for consistency. *)
  Hashtbl.remove t.trap_depths label;

  (* Return updated list of eliminated blocks.
     CR gyorsh: update this when transitively eliminate blocks. *)
  label::dead_blocks

(* Must be called after predecessors are registered
   and split labels are registered. *)
let rec eliminate_dead_blocks t =
  (* if not t.preserve_orig_labels then
   *   failwith "Won't eliminate dead blocks when preserve_orig_labels is set."; *)
  let found = Hashtbl.fold (fun label block found ->
    if (LabelSet.is_empty block.predecessors) &&
       (not (is_live_trap_handler t label)) &&
       (t.cfg.entry_label <> label) then
      label::found
    else found)
    t.cfg.blocks []
  in
  let num_found = List.length found in
  if num_found > 0 then begin
    let dead_blocks = List.fold_left (eliminate_dead_block t) [] found in
    let dead_blocks = List.sort_uniq Numbers.Int.compare dead_blocks in
    let num_eliminated = List.length dead_blocks in
    assert (num_eliminated >= num_found);
    if verbose then begin
      Printf.printf
        "Found %d dead blocks in function %s, eliminated %d (transitively).\n"
        num_found
        t.cfg.fun_name
        num_eliminated;
      Printf.printf "Eliminated blocks are:";
      List.iter (fun lbl -> Printf.printf "\n%d" lbl) dead_blocks;
      Printf.printf "\n"
    end;
    eliminate_dead_blocks t
  end


module M = Numbers.Int.Map
let simplify_terminator block =
  let t = block.terminator in
  match t.desc with
  | Branch successors ->
    (* Merge successors that go to the same label.
       Preserve order of successors, except
       successors that share the same label target are grouped. *)
    (* Map label to list of conditions that target it. *)
    (* CR gyorsh: pairwise join of conditions is not canonical,
       because some joins are not representable as a condition. *)
    let map = List.fold_left (fun map (c,l) ->
      let s = match M.find_opt l map with
        | None -> [c] (* Not seen this target yet *)
        | Some (c1::rest) -> (Simplify.disjunction c c1) @ rest
        | Some [] -> assert false
      in M.add l s map)
      M.empty
      successors in
    let (new_successors, map) =
      List.fold_left
        (fun (res, map) (_,l)  ->
           match M.find_opt l map with
           | None -> (res, map)
           | Some s ->
             let map = M.remove l map in
             let res = List.fold_left
                         (fun res c -> ((c,l)::res))
                         res s in
             (res, map))
        ([],map) successors in
    assert (M.is_empty map);
    let new_len = List.length new_successors in
    let len = List.length successors in
    assert (new_len <= len);
    if new_len < len then
      block.terminator <- { t with desc = Branch new_successors };
  | Switch labels ->
    (* Convert simple case to branches. *)
    (* Find position k and label l such that label.(j)=l for all j=k...len-1. *)
    let len = Array.length labels in
    assert (len > 0);
    let l = labels.(len - 1) in
    let rec find_pos k =
      if k = 0 then k
      else if labels.(k-1) = l then
        find_pos (k-1)
      else k
    in
    let k = find_pos (len-1) in
    begin match k with
    | 0 -> (* All labels are the same and equal to l *)
      block.terminator <- { t with desc = (Branch [(Always,l)] ) }
    | 1 ->
      let t0 = Test(Iinttest_imm(Iunsigned Clt, 1)) (* arg < 1  *) in
      let t1 = Test(Iinttest_imm(Isigned   Cge, 1)) (* arg >= 1 *) in
      block.terminator <- { t with desc = Branch [(t0,labels.(0));
                                                  (t1,l)] }
    | 2 ->
      let t0 = Test(Iinttest_imm(Iunsigned Clt, 1)) (* arg < 1  *) in
      let t1 = Test(Iinttest_imm(Iunsigned Ceq, 1)) (* arg = 1 *) in
      let t2 = Test(Iinttest_imm(Isigned   Cgt, 1)) (* arg > 1 *) in
      block.terminator <- { t with desc =  Branch [(t0,labels.(0));
                                                   (t1,labels.(1));
                                                   (t2,l)] }
    | _ -> ()
    end
  | _ -> ()

type fallthrough_block = { label:label;
                           target_label:label;
                         }
(* Disconnects fallthrough block by re-routing it predecessors
   to point directly to the successor block. *)
let disconnect_fallthrough_block t { label; target_label; } =
  let block = Hashtbl.find t.cfg.blocks label in
  (* Update the successor block's predecessors set:
     first remove the current block and then add its predecessors.  *)
  let target_block = Hashtbl.find t.cfg.blocks target_label in
  target_block.predecessors <- LabelSet.remove label
                                 target_block.predecessors;
  let update_pred pred_label =
    (* Update the predecessor block's terminators *)
    let replace_label l =
      if l = label then begin
        target_block.predecessors <- LabelSet.add pred_label
                                       target_block.predecessors;
        target_label
      end else l in
    let replace_successor (cond, l) = (cond, replace_label l) in
    let pred_block = Hashtbl.find t.cfg.blocks pred_label in
    let t = pred_block.terminator in begin
      match t.desc with
      | Branch successors ->
        let new_successors = List.map replace_successor successors in
        pred_block.terminator <- { t with desc = Branch new_successors }
      | Switch labels ->
        let new_labels = Array.map replace_label labels in
        pred_block.terminator <- { t with desc = Switch new_labels }
      | _ -> ()
    end;
    simplify_terminator pred_block
  in
  LabelSet.iter update_pred block.predecessors;
  block.terminator <- { block.terminator with desc = Branch [] };
  block.predecessors <- LabelSet.empty

(* Find and disconnect fallthrough blocks until fixpoint.
   Does not eliminate dead blocks that result from it.
   Dead block elimination should run after it to delete these blocks.*)
let rec disconnect_fallthrough_blocks t =
  let found = Hashtbl.fold (fun label block found ->
    let successors_labels = successor_labels block in
    if (t.cfg.entry_label <> label) &&             (* not entry block *)
       List.length successors_labels = 1 &&       (* single successor *)
       (not (is_live_trap_handler t label)) &&      (* not trap label *)
       List.length block.body = 0                       (* empty body *)
    then begin
      let target_label = List.hd successors_labels in
      if verbose then begin
        Printf.printf "block at %d has single successor %d\n"
          label target_label
      end;
      { label; target_label; }::found
    end else found)
    t.cfg.blocks []
  in
  let len = List.length found in
  if len > 0 then begin
    List.iter (disconnect_fallthrough_block t) found;
    if verbose then Printf.printf "Disconnected fallthrough blocks: %d\n" len;
    disconnect_fallthrough_blocks t
  end

let eliminate_fallthrough_blocks t =
  (* Find and disconnect fallthrough blocks (i.e., blocks with empty body
     and a single successor) by rerouting their predecessors
     to point directly to their successors. Then eliminate dead blocks,
     which can create new fallthrough blocks by eliminated successors.
     Repeat until fixpoint.
     Termination is guaranteed because every step eliminates an edge or a node.
     The order matters for performance but not for the final result.  *)
  let rec loop () =
    let len = Hashtbl.length t.cfg.blocks in
    disconnect_fallthrough_blocks t;
    eliminate_dead_blocks t;
    let new_len = Hashtbl.length t.cfg.blocks in
    if new_len < len && new_len > 0 then begin
      if verbose then
        Printf.printf "Eliminated %d fallthrough blocks in %s: len=%d new_len=%d\n"
          (len - new_len) t.cfg.fun_name len new_len;
      loop ()
    end
  in
  if verbose then print stdout t;
  Hashtbl.iter (fun _ b -> simplify_terminator b) t.cfg.blocks;
  loop ()

(* CR gyorsh: implement CFG traversal *)
(* CR gyorsh: abstraction of cfg updates that transparently and efficiently
   keeps predecessors and successors in sync. For example,
   change successors relation should automatically updates the predecessors
   relation without recomputing them from scratch. *)
