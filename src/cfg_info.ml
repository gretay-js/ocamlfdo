(* Create cfg_info *)
[@@@ocaml.warning "+a-30-40-41-42-44"]

open! Core
module Cfg = Ocamlcfg.Cfg
module BB = Cfg.Basic_block
module CL = Ocamlcfg.Cfg_with_layout

let verbose = ref true

type blocks = Block_info.t Hashtbl.M(Cfg_label).t [@@deriving sexp]

type t =
  { cl : CL.t
  ; func : Func.t
  ; mutable malformed_traces : Execount.t
        (** number of fallthrough traces that didn't correspond to the cfg *)
  ; blocks : blocks
  ; id_to_label : Cfg_label.t Hashtbl.M(Int).t
        (** Map id of instruction to label of the block that contains the
            instruction. Used for mapping perf data back to linear IR. *)
  }

let get_block t label = Hashtbl.find t.blocks label
let malformed_traces t = t.malformed_traces

let compute_id_to_label t =
  let f label block =
    List.iter
      ~f:(fun (i : _ Cfg.instruction) ->
        Hashtbl.add_exn t.id_to_label ~key:i.id ~data:label)
      (BB.body block);
    let terminator = BB.terminator block in
    Hashtbl.add_exn t.id_to_label ~key:terminator.id ~data:label
  in
  Cfg.iter_blocks (CL.cfg t.cl) ~f
;;

let create cl func =
  let t =
    { cl
    ; func
    ; malformed_traces = 0L
    ; blocks = Hashtbl.create (module Cfg_label)
    ; id_to_label = Hashtbl.create (module Cfg_label)
    }
  in
  compute_id_to_label t;
  t
;;

let id_to_label t id =
  match Hashtbl.find t.id_to_label id with
  | None ->
    Report.(
      user_error
        ~hint:(Some Hint.Mismatch)
        "Cannot find CFG label for Linear ID %d in func %d"
        id
        t.func.id)
  | Some lbl ->
    if !verbose then Printf.printf "Found label %d for id %d in map\n" lbl id;
    Some lbl
;;

let terminator_to_string cfg block =
  let n = Cfg.successor_labels cfg block |> List.length in
  match (BB.terminator block).desc with
  | Return -> "Return"
  | Raise _ -> "Raise"
  | Tailcall (Self _) -> "Tailcall self"
  | Tailcall (Func _) -> "Tailcall"
  | Never ->
    Report.user_error
      "Illegal cfg for %s: block %d terminator is Never"
      (Cfg.fun_name cfg)
      (BB.start block)
  | Always _ | Parity_test _ | Truth_test _ | Float_test _ | Int_test _ ->
    sprintf "Branch with %d successors" n
  | Switch s ->
    sprintf "Switch with %d case and %d distinct successors" (Array.length s) n
;;

let mal t count =
  if !verbose then printf "Malformed trace with %Ld counts.\n" count;
  t.malformed_traces <- Execount.(t.malformed_traces + count)
;;

let first_id (block : BB.t) =
  match BB.body block with
  | [] -> (BB.terminator block).id
  | hd :: _ -> hd.id
;;

(* Some Linear instructions don't generate any code on some platforms. There
   might not be any execounters associated with them.

   Check that [id] can be the first instruction emitted in this block, even
   though the actual first instruction in Linear IR may be different.

   This is used as a safety check when mapping execution counters to blocks.

   We don't know what the backends actually generate, so this check is only
   an approximation. At the time of writing this, these are the only
   instructions that might not result in any code, on some backend.

   Some of these instructions result in dwarf info (such as cfi directives),
   and may even result in a debug .loc line that is not associated with any
   assembly instruction and may bedirectly followed by another .loc.

   Both .loc will be associated with the same code address in Dwarf debug
   line programs, but usually only the last one is recorded in the profile
   (and likely will be, as it is the closed one associated with the address,
   although both .loc will be associated with the same address). *)
let can_be_first_emitted_id id (block : BB.t) =
  let open Cfg in
  let rec check_first body =
    match body with
    | [] -> (BB.terminator block).id = id
    | hd :: tl ->
      if hd.id = id
      then true
      else (
        match hd.desc with
        | Reloadretaddr | Prologue | Poptrap -> check_first tl
        | Op _ | Call _ | Pushtrap _ -> false)
  in
  check_first (BB.body block)
;;

let get_or_add_block t (block : BB.t) =
  Hashtbl.find_or_add t.blocks (BB.start block) ~default:(fun () ->
      let terminator = BB.terminator block in
      let terminator_id =
        match terminator.id with
        | 0 ->
          (* use the id of the last instruction in the body *)
          (match terminator.desc with
          | Always _ -> assert true
          | Never
          | Return
          | Parity_test _
          | Truth_test _
          | Float_test _
          | Int_test _
          | Switch _
          | Raise _
          | Tailcall _ -> assert false);
          let last = List.last_exn (BB.body block) in
          last.id
        | n -> n
      in
      let first_id = first_id block in
      let block_start = BB.start block in
      if !verbose
      then
        printf
          "make new block info for block.start=%d first_id=%d terminator_id=%d\n"
          block_start
          first_id
          terminator_id;
      Block_info.mk ~label:block_start ~first_id ~terminator_id)
;;

let record t block ~count =
  let b = get_or_add_block t block in
  Block_info.add b ~count
;;

let get_linearid (loc : Loc.t) = Option.value_exn loc.dbg

(* Find basic instruction whose id=[linearid] in [block] *)
let _get_basic_instr linearid (block : BB.t) =
  List.find (BB.body block) ~f:(fun instr -> instr.id = linearid)
;;

(* Find basic instruction right before the one with [linearid] in [block]. If
   not found linearid or linearid is the first instruction, then return None. *)
exception Found_prev of Cfg.basic Cfg.instruction option

let _prev_instr linearid block =
  let open Cfg in
  try
    ignore
      ( List.fold (BB.body block) ~init:None ~f:(fun prev instr ->
            if instr.id = linearid then raise (Found_prev prev) else Some instr)
        : Cfg.basic Cfg.instruction option );
    None
  with
  | Found_prev prev -> prev
;;

(* Find the block in [cfg] that contains [loc] using its linearid *)
let get_block_for_loc t (loc : Loc.t) =
  match loc.dbg with
  | None ->
    if !verbose
    then (
      let rel = Option.value_exn loc.rel in
      printf "No linearid for loc in func %d at offsets %d\n" rel.id rel.offset);
    None
  | Some dbg ->
    (match id_to_label t dbg with
    | None -> Report.user_error "No cfg label for linearid %d in %d" dbg t.func.id ()
    | Some label ->
      (match Cfg.get_block (CL.cfg t.cl) label with
      | Some block -> Some block
      | None ->
        Report.user_error
          "Can't find cfg basic block labeled %d for linearid %d in func %d\n"
          label
          dbg
          t.func.id
          ()))
;;

let record_intra t ~from_loc ~to_loc ~count ~mispredicts =
  let from_block = get_block_for_loc t from_loc in
  let to_block = get_block_for_loc t to_loc in
  match from_block, to_block with
  | None, None ->
    if !verbose then printf "Ignore intra branch count %Ld, can't map to CFG\n" count
  | Some from_block, None ->
    if !verbose
    then printf "Ignore intra branch count %Ld, can't map target to CFG\n" count;
    record t from_block ~count
  | None, Some to_block ->
    if !verbose
    then printf "Ignore intra branch count %Ld, can't map source to CFG\n" count;
    record t to_block ~count
  | Some from_block, Some to_block ->
    record t from_block ~count;
    record t to_block ~count;
    let from_linearid = get_linearid from_loc in
    let to_linearid = get_linearid to_loc in
    let to_block_first_id = first_id to_block in
    let to_block_start = BB.start to_block in
    let from_block_start = BB.start from_block in
    if !verbose
    then
      printf
        "Intra branch count %Ld from (id=%d,lbl=%d) to (id=%d,lbl=%d,first_id=%d)\n"
        count
        from_linearid
        from_block_start
        to_linearid
        to_block_start
        to_block_first_id;
    let bi = get_or_add_block t from_block in
    let b =
      { Block_info.target = Some to_loc
      ; target_label = Some to_block_start
      ; target_id = Some to_block_first_id
      ; intra = true
      ; fallthrough = false
      ; taken = count
      ; mispredicts
      }
    in
    let from_block_terminator = BB.terminator from_block in
    if from_block_terminator.id = from_linearid
    then (
      match (* Find the corresponding successor *)
            from_block_terminator.desc with
      | Return ->
        (* return from a recursive call *)
        (* target must be right after a call *)
        if !verbose
        then
          printf "Return from (label=%d) to (label=%d)" from_block_start to_block_start
      (* CR-someday gyorsh: We could count the reverse of this edge as a call.
           It's a bit tricky as we need to find the instruction right before
           the to_loc and that is the callsite, and we need to construct
           callee from the entry location of the current function
           (cfg.entry_label). This will count the same call twice if both
           call and return are sampled. We should try to discard matching
           counts. *)
      (* let call_site = callsite_of_return_site to_loc
         * let cbi = get_or_add_block t (get_block call_site)
         * let callee = { b with
         *                (* start of this function *)
         *                target_label = cfg.entry_label;
         *                target = Some loc?;
         *              } in
         * add_call cbi ~callsite ~callee *)
      | Tailcall (Self _) ->
        if !verbose
        then
          printf
            "Tailcall from linid=%d from_label=%d %Ld"
            from_linearid
            from_block_start
            count;
        assert (Cfg.fun_tailrec_entry_point_label (CL.cfg t.cl) = to_block_start);
        (* CR-someday gyorsh: count calls *)
        (* Block_info.add_call bi ~callsite:from_loc ~callee:b; *)
        Block_info.add_branch bi b
      | Tailcall (Func _) -> assert false
      | Raise _ ->
        (* target must be a handler block *)
        (* assert (Cfg_builder.is_trap_handler cfg to_block.start) *)
        ()
      | Always _
      | Never
      | Parity_test _
      | Truth_test _
      | Float_test _
      | Int_test _
      | Switch _ ->
        assert (
          to_block_first_id = to_linearid || can_be_first_emitted_id to_linearid to_block);
        let successors = Cfg.successor_labels (CL.cfg t.cl) from_block in
        assert (List.mem successors to_block_start ~equal:Int.equal);
        Block_info.add_branch bi b)
    else
      ( (* CR-someday gyorsh: record calls *)
        (* recursive call, find the call instruction *) )
;;

let record_exit t (from_loc : Loc.t) (to_loc : Loc.t option) count mispredicts =
  (* Branch going outside of this function. *)
  match get_block_for_loc t from_loc with
  | None ->
    if !verbose then printf "Ignore inter branch count %Ld. Can't map to CFG.\n" count
  | Some from_block ->
    record t from_block ~count;
    (* Find the corresponding instruction and update its counters. The
         instruction is either a terminator or a call.*)
    let linearid = get_linearid from_loc in
    let terminator = BB.terminator from_block in
    let bi = get_or_add_block t from_block in
    if terminator.id = linearid
    then (
      match (* terminator *)
            terminator.desc with
      | Never ->
        Report.user_error
          "Illegal cfg for block %d: terminator is Never"
          (BB.start from_block)
      | Always _ | Parity_test _ | Truth_test _ | Float_test _ | Int_test _ | Switch _
      | Tailcall (Self _) ->
        (* can't branch outside the current function *)
        if !verbose
        then
          printf
            "record_exit count=%Ld from_block.start=%d terminator_id=%d.\n"
            count
            (BB.start from_block)
            terminator.id;
        assert false
      | Tailcall (Func _)
      | Return | Raise _ ->
        (match to_loc with
        | None -> ()
        | Some _ ->
          let b =
            { Block_info.target = to_loc
            ; target_label = None
            ; target_id = None
            ; intra = false
            ; fallthrough = false
            ; taken = count
            ; mispredicts
            }
          in
          Block_info.add_branch bi b))
    else ( (* Call *)
           (* CR-someday gyorsh: record calls *) )
;;

let record_entry t (to_loc : Loc.t) count _mispredicts =
  (* Branch into this function from another function, which may be unknown.
     One of the following situations:

     Callee: branch target is the first instr in the entry block.

     Return from call: branch target is a label after the call site.

     Exception handler: branch target is a trap handler. *)
  match get_block_for_loc t to_loc with
  | None ->
    if !verbose then printf "Ignore inter branch count %Ld. Can't map to CFG.\n" count
  | Some to_block ->
    (* CR-someday gyorsh: find the corresponding instruction and update its
         counters.*)
    record t to_block ~count
;;

(* Depending on the settings of perf record and the corresponding CPU
   configuration, LBR may capture different kinds of branches, including
   function calls and returns. *)
let record_branch
   t
   ~(from_loc : Loc.t option)
   ~(to_loc : Loc.t option)
   ~data:count
   ~mispredicts
  =
  (* at least one of the locations is known to be in this function *)
  match from_loc, to_loc with
  | None, None ->
    Report.user_error
      "Malformed profile: branch with both source and target locations unknown"
  | None, Some to_loc ->
    (match to_loc.rel with
    | Some rel -> if rel.id = t.func.id then record_entry t to_loc count mispredicts
    | _ -> ())
  | Some from_loc, None ->
    (match from_loc.rel with
    | Some rel ->
      if rel.id = t.func.id then record_exit t from_loc to_loc count mispredicts
    | _ -> ())
  | Some from_loc, Some to_loc ->
    (match from_loc.rel, to_loc.rel with
    | Some from_rel, Some to_rel
      when from_rel.id = t.func.id && to_rel.id = t.func.id ->
      record_intra t ~from_loc ~to_loc ~count ~mispredicts
    | Some from_rel, _
      when from_rel.id = t.func.id ->
      record_exit t from_loc (Some to_loc) count mispredicts
    | _, Some to_rel
      when to_rel.id = t.func.id -> record_entry t to_loc count mispredicts
    | _ -> assert false)
;;

exception Malformed_fallthrough_trace

let compute_fallthrough_execounts t from_lbl to_lbl count =
  let layout = CL.layout t.cl in
  let cfg = CL.cfg t.cl in
  (* Get the part of the layout starting from from_block up to but not
     including to_block *)
  try
    let fallthrough = List.drop_while ~f:(fun lbl -> not (lbl = from_lbl)) layout in
    let fallthrough =
      match List.findi fallthrough ~f:(fun _ lbl -> lbl = to_lbl) with
      | None -> raise Malformed_fallthrough_trace
      | Some (to_pos, _) -> List.take fallthrough to_pos
    in
    if !verbose
    then (
      printf
        "func %d trace (from_lbl=%d,to_lbl=%d)\n fallthrough: "
        t.func.id
        from_lbl
        to_lbl;
      List.iter fallthrough ~f:(fun lbl -> printf " %d" lbl);
      printf "\nlayout=";
      List.iter layout ~f:(fun lbl -> printf " %d" lbl);
      printf "\n");
    (* Check that all terminators fall through *)
    let check_fallthrough src_lbl dst_lbl =
      let block = Option.value_exn (Cfg.get_block cfg src_lbl) in
      let desc = (BB.terminator block).desc in
      match desc with
      | Never ->
        Report.user_error
          "Illegal cfg for block %d: terminator is Never"
          (BB.start block)
      | Always _ | Parity_test _ | Truth_test _ | Float_test _ | Int_test _ | Switch _ ->
        if !verbose
        then (
          printf
            "check_fallthrough in func %d trace (from_lbl=%d,to_lbl=%d): src_lbl=%d \
             dst_lbl=%d\n\
             src_block.successor_labels:\n"
            t.func.id
            from_lbl
            to_lbl
            src_lbl
            dst_lbl;
          List.iter (Cfg.successor_labels cfg block) ~f:(fun lbl -> printf "%d\n" lbl));
        if List.mem (Cfg.successor_labels cfg block) dst_lbl ~equal:Int.equal
        then src_lbl
        else (
          if !verbose
          then
            printf
              "Malformed fallthrough in func %d trace (from_lbl=%d,to_lbl=%d): \
               src_lbl=%d dst_lbl=%d\n\
               src_block.successor_labels:\n"
              t.func.id
              from_lbl
              to_lbl
              src_lbl
              dst_lbl;
          raise Malformed_fallthrough_trace)
      | Return | Raise _ | Tailcall _ ->
        if !verbose
        then
          printf
            "Unexpected terminator %s in fallthrough trace func %d \
             (from_lbl=%d,to_lbl=%d): src_lbl=%d dst_lbl=%d\n\
             src_block.successor_labels:\n"
            (terminator_to_string cfg block)
            t.func.id
            from_lbl
            to_lbl
            src_lbl
            dst_lbl;
        raise Malformed_fallthrough_trace
    in
    assert (from_lbl = List.fold_right fallthrough ~init:to_lbl ~f:check_fallthrough);
    (* Account only for the intermediate blocks in the trace. Endpoint blocks
       of the trace are accounted for when we handled their LBR branches. *)
    let record_fallthrough src_lbl dst_lbl =
      if !verbose
      then printf "record_fallthrough %d->%d count=%Ld\n" src_lbl dst_lbl count;
      let dst_block = Option.value_exn (Cfg.get_block cfg dst_lbl) in
      record t dst_block ~count;
      let target_bi = get_or_add_block t dst_block in
      let src_block = Option.value_exn (Cfg.get_block cfg src_lbl) in
      let bi = get_or_add_block t src_block in
      let b =
        { Block_info.target = None
        ; target_label = Some dst_lbl
        ; target_id = Some target_bi.first_id
        ; intra = true
        ; fallthrough = true
        ; taken = count
        ; mispredicts = 0L
        }
      in
      if !verbose
      then
        printf
          !"record_fallthrough: add b=%{sexp:Block_info.b}\nto bi=%{sexp:Block_info.t}\n"
          b
          bi;
      Block_info.add_branch bi b;
      if !verbose
      then printf !"record_fallthrough: after add:\nbi=%{sexp:Block_info.t}\n" bi;
      src_lbl
    in
    assert (from_lbl = List.fold_right fallthrough ~init:to_lbl ~f:record_fallthrough);
    if !verbose
    then printf "recorded healthy trace from %d to %d count %Ld\n" from_lbl to_lbl count
    (* printf !"%{sexp:t}\n" t.blocks *)
  with
  | Malformed_fallthrough_trace ->
    (* If the trace is malformed, don't add counts *)
    mal t count
;;

let record_trace t ~(from_loc : Loc.t option) ~(to_loc : Loc.t option) ~data:count =
  (* both locations must be in this function *)
  match from_loc, to_loc with
  | None, _
  | _, None ->
    if !verbose
    then
      printf
        "Ignoring trace with count %Ld to or from function is not the same or not known.\n"
        count;
    mal t count
  | Some from_loc, Some to_loc ->
    (match from_loc.rel, to_loc.rel with
    | Some from_rel, Some to_rel
      when from_rel.id = t.func.id && to_rel.id = t.func.id ->
      (match get_block_for_loc t from_loc, get_block_for_loc t to_loc with
      | Some from_block, Some to_block
        when BB.start from_block = BB.start to_block ->
        if !verbose
        then
          printf "No fallthroughs in trace with count %Ld:from_block = to_block\n" count;
        record t from_block ~count
      | Some from_block, Some to_block ->
        if !verbose
        then
          printf
            "trace=> (from_linid=%d,to_linid=%d)=> (from_block=%d,to_block=%d)\n"
            (Option.value_exn from_loc.dbg)
            (Option.value_exn to_loc.dbg)
            (BB.start from_block)
            (BB.start to_block);
        compute_fallthrough_execounts t (BB.start from_block) (BB.start to_block) count
      | _ ->
        if !verbose
        then
          printf
            "Ignoring trace with count %Ld:cannot map to_loc or from_loc to cfg blocks.\n"
            count;
        mal t count)
    | _ ->
      if !verbose
      then
        printf
          "Ignoring trace with count %Ld to or from function is not the same or not \
           known.\n"
          count;
      mal t count)
;;

let record_ip t ~loc ~data:count =
  match loc with
  | None -> if !verbose then printf "Ignore exec count \n, can't find location\n"
  | Some loc ->
    (match get_block_for_loc t loc with
    | None -> if !verbose then printf "Ignore exec count \n, can't map to cfg\n"
    | Some block -> record t block ~count)
;;

(** debug printing functions *)

let dump_branch (b : Block_info.b) =
  match b.target_label with
  | None -> ()
  | Some successor ->
    if b.intra
    then
      Printf.printf
        "(.L%d%s:%Ld%s)"
        successor
        (if b.fallthrough then " tr" else "")
        b.taken
        (if Execount.(b.mispredicts > 0L) then sprintf " mis:%Ld" b.mispredicts else "")
;;

let dump_call t (c : Block_info.c) =
  let rel =
    match c.callsite.rel with
    | Some rel -> rel
    | None ->
      Report.user_error "Malformed cfg_info for func %d: callsite missing" t.func.id
  in
  if not (t.func.id = rel.id)
  then
    Report.user_error
      "Malformed cfg_info for func %d: callsite mismatch function id %d"
      t.func.id
      rel.id;
  match c.callsite.dbg with
  | None ->
    Report.user_error
      "Malformed cfg_info for func %d: callsite without linear id at offset %d"
      t.func.id
      rel.offset
  | Some linearid ->
    Printf.printf "\tcallsite %d: " linearid;
    List.iter c.callees ~f:dump_branch
;;

let dump_block t label =
  Printf.printf ".L%d: " label;
  match get_block t label with
  | None -> Printf.printf "\n"
  | Some b ->
    Printf.printf "%Ld " b.count;
    List.iter b.branches ~f:dump_branch;
    List.iter b.calls ~f:(dump_call t);
    Printf.printf "\n"
;;

let dump t =
  Printf.printf "Cfg info for func %d: %Ld\n" t.func.id t.func.count;
  let layout = CL.layout t.cl in
  List.iter layout ~f:(dump_block t)
;;

let dump_dot t msg =
  let annotate_block label =
    match get_block t label with
    | None -> ""
    | Some bi -> sprintf "%Ld" bi.count
  in
  let annotate_succ label succ =
    match get_block t label with
    | None -> ""
    | Some bi ->
      (match
         List.find bi.branches ~f:(fun b ->
             match b.target_label with
             | None -> false
             | Some target -> target = succ)
       with
      | None -> ""
      | Some b ->
        assert b.intra;
        Printf.sprintf
          "%s%Ld%s"
          (if b.fallthrough then "tr\\r" else "")
          b.taken
          (if Execount.(b.mispredicts > 0L)
          then sprintf "\\rmis:%Ld" b.mispredicts
          else ""))
  in
  CL.save_as_dot t.cl ~show_instr:false ~annotate_block ~annotate_succ msg
;;

let fold t = Hashtbl.fold t.blocks
let iter t = Hashtbl.iter t.blocks
