(* Create cfg_info *)
[@@@ocaml.warning "+a-4-30-40-41-42-44"]

open! Core
module Cfg = Ocamlcfg.Cfg
module BB = Cfg.Basic_block
module CL = Ocamlcfg.Cfg_with_layout

let verbose = ref true

type blocks = Block_info.t Hashtbl.M(Cfg_label).t [@@deriving sexp]

type t =
  { cl : CL.t;
    func : Func.t;
    mutable malformed_traces : Execount.t;
        (** number of fallthrough traces that didn't correspond to the cfg *)
    blocks : blocks;
    id_to_label : Cfg_label.t Hashtbl.M(Int).t
        (** Map id of instruction to label of the block that contains the
            instruction. Used for mapping perf data back to linear IR. *)
  }

let blocks t = t.blocks

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

let create cl func =
  let t =
    { cl;
      func;
      malformed_traces = 0L;
      blocks = Hashtbl.create (module Cfg_label);
      id_to_label = Hashtbl.create (module Cfg_label)
    }
  in
  compute_id_to_label t;
  t

let id_to_label t id =
  match Hashtbl.find t.id_to_label id with
  | None -> Misc.fatal_errorf "Cannot find label for ID %d in id_to_label" id
  | Some lbl ->
      if !verbose then
        Printf.printf "Found label %d for id %d in map\n" lbl id;
      Some lbl

let terminator_to_string (t : Cfg.terminator) =
  match t with
  | Return -> "Return"
  | Raise _ -> "Raise"
  | Tailcall _ -> "Tailcall"
  | Branch s -> sprintf "Branch with %d successors" (List.length s)
  | Switch s -> sprintf "Switch with %d successors" (Array.length s)

let mal t count =
  if !verbose then printf "Malformed trace with %Ld counts.\n" count;
  t.malformed_traces <- Int64.(t.malformed_traces + count)

let first_id (block : BB.t) =
  match BB.body block with
  | [] -> (BB.terminator block).id
  | hd :: _ -> hd.id

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
    | hd :: tl -> (
        if hd.id = id then true
        else
          match hd.desc with
          | Reloadretaddr | Prologue | Poptrap -> check_first tl
          | _ -> false )
  in
  check_first (BB.body block)

let get_block_info t (block : BB.t) =
  Hashtbl.find_or_add t.blocks (BB.start block) ~default:(fun () ->
      let terminator = BB.terminator block in
      let terminator_id =
        match terminator.id with
        | 0 ->
            (* use the id of the last instruction in the body *)
            ( match terminator.desc with
            | Branch [(Always, _)] -> assert true
            | _ -> assert false );
            let last = List.last_exn (BB.body block) in
            last.id
        | n -> n
      in
      let first_id = first_id block in
      let block_start = BB.start block in
      if !verbose then
        printf
          "make new block info for block.start=%d first_id=%d \
           terminator_id=%d\n"
          block_start first_id terminator_id;
      Block_info.mk ~label:block_start ~first_id ~terminator_id)

let record t block ~count =
  let b = get_block_info t block in
  Block_info.add b ~count

let get_linearid (loc : Loc.t) = Option.value_exn loc.dbg

(* Find basic instruction whose id=[linearid] in [block] *)
let _get_basic_instr linearid (block : BB.t) =
  List.find (BB.body block) ~f:(fun instr -> instr.id = linearid)

(* Find basic instruction right before the one with [linearid] in [block]. If
   not found linearid or linearid is the first instruction, then return None. *)
exception Found_prev of Cfg.basic Cfg.instruction option

let _prev_instr linearid block =
  let open Cfg in
  try
    ignore
      ( List.fold (BB.body block) ~init:None ~f:(fun prev instr ->
            if instr.id = linearid then raise (Found_prev prev)
            else Some instr)
        : Cfg.basic Cfg.instruction option );
    None
  with Found_prev prev -> prev

(* Find the block in [cfg] that contains [loc] using its linearid *)
let get_block t (loc : Loc.t) =
  match loc.dbg with
  | None ->
      ( if !verbose then
        let rel = Option.value_exn loc.rel in
        printf "No linearid for 0x%Lx in func %d at offsets %d\n" loc.addr
          rel.id rel.offset );
      None
  | Some dbg -> (
      match id_to_label t dbg with
      | None ->
          Report.user_error "No cfg label for linearid %d in %d" dbg
            t.func.id ()
      | Some label -> (
          match Cfg.get_block (CL.cfg t.cl) label with
          | Some block -> Some block
          | None ->
              Report.user_error
                "Can't find cfg basic block labeled %d for linearid %d in \
                 func %d\n"
                label dbg t.func.id () ) )

let record_intra t ~from_loc ~to_loc ~count ~mispredicts =
  let from_block = get_block t from_loc in
  let to_block = get_block t to_loc in
  match (from_block, to_block) with
  | None, None ->
      if !verbose then
        printf
          "Ignore intra branch count %Ld from 0x%Lx to 0x%Lx, can't map to \
           CFG\n"
          count from_loc.addr to_loc.addr
  | Some from_block, None ->
      if !verbose then
        printf
          "Ignore intra branch count %Ld from 0x%Lx to 0x%Lx, can't map \
           target to CFG\n"
          count from_loc.addr to_loc.addr;
      record t from_block ~count
  | None, Some to_block ->
      if !verbose then
        printf
          "Ignore intra branch count %Ld from 0x%Lx to 0x%Lx, can't map \
           source to CFG\n"
          count from_loc.addr to_loc.addr;
      record t to_block ~count
  | Some from_block, Some to_block ->
      record t from_block ~count;
      record t to_block ~count;
      let from_linearid = get_linearid from_loc in
      let to_linearid = get_linearid to_loc in
      let to_block_first_id = first_id to_block in
      let to_block_start = BB.start to_block in
      let from_block_start = BB.start from_block in
      if !verbose then
        printf
          "Intra branch count %Ld from 0x%Lx (id=%d,lbl=%d) to 0x%Lx \
           (id=%d,lbl=%d,first_id=%d)\n"
          count from_loc.addr from_linearid from_block_start to_loc.addr
          to_linearid to_block_start to_block_first_id;
      let bi = get_block_info t from_block in
      let b =
        { Block_info.target = Some to_loc;
          target_label = Some to_block_start;
          target_id = Some to_block_first_id;
          intra = true;
          fallthrough = false;
          taken = count;
          mispredicts
        }
      in
      let from_block_terminator = BB.terminator from_block in
      if from_block_terminator.id = from_linearid then (
        (* Find the corresponding successor *)
        match from_block_terminator.desc with
        | Return ->
            (* return from a recursive call *)
            (* target must be right after a call *)
            if !verbose then
              printf "Return from (label=%d) to (label=%d)" from_block_start
                to_block_start
        (* CR-soon gyorsh: We could count the reverse of this edge as a call.
           It's a bit tricky as we need to find the instruction right before
           the to_loc and that is the callsite, and we need to construct
           callee from the entry location of the current function
           (cfg.entry_label). This will count the same call twice if both
           call and return are sampled. We should try to discard matching
           counts. *)
        (* let call_site = callsite_of_return_site to_loc
         * let cbi = get_block_info t (get_block call_site)
         * let callee = { b with
         *                (* start of this function *)
         *                target_label = cfg.entry_label;
         *                target = Some loc?;
         *              } in
         * add_call cbi ~callsite ~callee *)
        | Tailcall _ ->
            if !verbose then
              printf "Tailcall from linid=%d from_label=%d %Ld" from_linearid
                from_block_start count;
            Block_info.add_call bi ~callsite:from_loc ~callee:b
        | Raise _ ->
            (* target must be a handler block *)
            (* assert (Cfg_builder.is_trap_handler cfg to_block.start) *)
            ()
        | Branch _ | Switch _ ->
            assert (
              to_block_first_id = to_linearid
              || can_be_first_emitted_id to_linearid to_block );
            let successors = Cfg.successor_labels (CL.cfg t.cl) from_block in
            assert (List.mem successors to_block_start ~equal:Int.equal);
            Block_info.add_branch bi b )
      else
        ( (* CR-someday gyorsh: record calls *)
          (* recursive call, find the call instruction *) )

let record_exit t (from_loc : Loc.t) (to_loc : Loc.t) count mispredicts =
  (* Branch going outside of this function. *)
  match get_block t from_loc with
  | None ->
      if !verbose then
        printf
          "Ignore inter branch count %Ld from 0x%Lx. Can't map to CFG.\n"
          count from_loc.addr
  | Some from_block ->
      record t from_block ~count;

      (* Find the corresponding instruction and update its counters. The
         instruction is either a terminator or a call.*)
      let linearid = get_linearid from_loc in
      let terminator = BB.terminator from_block in
      let bi = get_block_info t from_block in
      if terminator.id = linearid then
        (* terminator *)
        match terminator.desc with
        | Branch _ | Switch _ | Tailcall (Self _) ->
            (* can't branch outside the current function *)
            if !verbose then
              printf
                "record_exit count=%Ld 0x%Lx->0x%Lx from_block.start=%d \
                 terminator_id=%d.\n"
                count from_loc.addr to_loc.addr (BB.start from_block)
                terminator.id;
            assert false
        | Tailcall (Func _) | Return | Raise _ ->
            let b =
              { Block_info.target = Some to_loc;
                target_label = None;
                target_id = None;
                intra = false;
                fallthrough = false;
                taken = count;
                mispredicts
              }
            in
            Block_info.add_branch bi b
      else ( (* Call *)
             (* CR-someday gyorsh: record calls *) )

let record_entry t (from_loc : Loc.t) (to_loc : Loc.t) count _mispredicts =
  (* Branch into this function from another function, which may be unknown.
     One of the following situations:

     Callee: branch target is the first instr in the entry block.

     Return from call: branch target is a label after the call site.

     Exception handler: branch target is a trap handler. *)
  if Int64.(from_loc.addr < 0L) then (
    if
      (* [from_loc.addr] of this branch does not belong to the binary and
         usually to_addr cannot be target of jump because it is not a return
         from a call instruction and not an entry function. Could it be a
         context switch? *)
      !verbose
    then
      printf "record_entry at 0x%Lx -> 0x%Lx (from_addr < 0)\n" from_loc.addr
        to_loc.addr )
  else
    match get_block t to_loc with
    | None ->
        if !verbose then
          printf
            "Ignore inter branch count %Ld to 0x%Lx. Can't map to CFG.\n"
            count to_loc.addr
    | Some to_block ->
        (* CR-someday gyorsh: find the corresponding instruction and update
           its counters.*)
        record t to_block ~count

(* Depending on the settings of perf record and the corresponding CPU
   configuration, LBR may capture different kinds of branches, including
   function calls and returns. *)
let record_branch t ~(from_loc : Loc.t) ~(to_loc : Loc.t) ~data:count
    ~mispredicts =
  (* at least one of the locations is known to be in this function *)
  match (from_loc.rel, to_loc.rel) with
  | Some from_rel, Some to_rel
    when from_rel.id = t.func.id && to_rel.id = t.func.id ->
      record_intra t ~from_loc ~to_loc ~count ~mispredicts
  | Some from_rel, _ when from_rel.id = t.func.id ->
      record_exit t from_loc to_loc count mispredicts
  | _, Some to_rel when to_rel.id = t.func.id ->
      record_entry t from_loc to_loc count mispredicts
  | _ -> assert false

exception Malformed_fallthrough_trace

let compute_fallthrough_execounts t from_lbl to_lbl count =
  let layout = CL.layout t.cl in
  let cfg = CL.cfg t.cl in
  (* Get the part of the layout starting from from_block up to but not
     including to_block *)
  try
    let fallthrough =
      List.drop_while ~f:(fun lbl -> not (lbl = from_lbl)) layout
    in
    let fallthrough =
      match List.findi fallthrough ~f:(fun _ lbl -> lbl = to_lbl) with
      | None -> raise Malformed_fallthrough_trace
      | Some (to_pos, _) -> List.take fallthrough to_pos
    in
    if !verbose then (
      printf "func %d trace (from_lbl=%d,to_lbl=%d)\n fallthrough: "
        t.func.id from_lbl to_lbl;
      List.iter fallthrough ~f:(fun lbl -> printf " %d" lbl);
      printf "\nlayout=";
      List.iter layout ~f:(fun lbl -> printf " %d" lbl);
      printf "\n" );

    (* Check that all terminators fall through *)
    let check_fallthrough src_lbl dst_lbl =
      let block = Option.value_exn (Cfg.get_block cfg src_lbl) in
      let desc = (BB.terminator block).desc in
      match desc with
      | Branch _ | Switch _ ->
          if !verbose then (
            printf
              "check_fallthrough in func %d trace (from_lbl=%d,to_lbl=%d): \
               src_lbl=%d dst_lbl=%d\n\
               src_block.successor_labels:\n"
              t.func.id from_lbl to_lbl src_lbl dst_lbl;
            List.iter (Cfg.successor_labels cfg block) ~f:(fun lbl ->
                printf "%d\n" lbl) );
          if
            List.mem
              (Cfg.successor_labels cfg block)
              dst_lbl ~equal:Int.equal
          then src_lbl
          else (
            if !verbose then
              printf
                "Malformed fallthrough in func %d trace \
                 (from_lbl=%d,to_lbl=%d): src_lbl=%d dst_lbl=%d\n\
                 src_block.successor_labels:\n"
                t.func.id from_lbl to_lbl src_lbl dst_lbl;
            raise Malformed_fallthrough_trace )
      | _ ->
          if !verbose then
            printf
              "Unexpected terminator %s in fallthrough trace func %d \
               (from_lbl=%d,to_lbl=%d): src_lbl=%d dst_lbl=%d\n\
               src_block.successor_labels:\n"
              (terminator_to_string desc)
              t.func.id from_lbl to_lbl src_lbl dst_lbl;
          raise Malformed_fallthrough_trace
    in
    assert (
      from_lbl
      = List.fold_right fallthrough ~init:to_lbl ~f:check_fallthrough );

    (* Account only for the intermediate blocks in the trace. Endpoint blocks
       of the trace are accounted for when we handled their LBR branches. *)
    let record_fallthrough src_lbl dst_lbl =
      if !verbose then
        printf "record_fallthrough %d->%d count=%Ld\n" src_lbl dst_lbl count;
      let dst_block = Option.value_exn (Cfg.get_block cfg dst_lbl) in
      record t dst_block ~count;
      let target_bi = get_block_info t dst_block in
      let src_block = Option.value_exn (Cfg.get_block cfg src_lbl) in
      let bi = get_block_info t src_block in
      let b =
        { Block_info.target = None;
          target_label = Some dst_lbl;
          target_id = Some target_bi.first_id;
          intra = true;
          fallthrough = true;
          taken = count;
          mispredicts = 0L
        }
      in
      if !verbose then
        printf
          !"record_fallthrough: add b=%{sexp:Block_info.b}\n\
            to bi=%{sexp:Block_info.t}\n"
          b bi;
      Block_info.add_branch bi b;
      if !verbose then
        printf
          !"record_fallthrough: after add:\nbi=%{sexp:Block_info.t}\n"
          bi;
      src_lbl
    in
    assert (
      from_lbl
      = List.fold_right fallthrough ~init:to_lbl ~f:record_fallthrough );
    if !verbose then
      printf "recorded healthy trace from %d to %d count %Ld\n" from_lbl
        to_lbl count
    (* printf !"%{sexp:t}\n" t.blocks *)
  with Malformed_fallthrough_trace ->
    (* If the trace is malformed, don't add counts *)
    mal t count

let record_trace t ~(from_loc : Loc.t) ~(to_loc : Loc.t) ~data:count =
  (* both locations must be in this function *)
  match (from_loc.rel, to_loc.rel) with
  | Some from_rel, Some to_rel
    when from_rel.id = t.func.id && to_rel.id = t.func.id -> (
      match (get_block t from_loc, get_block t to_loc) with
      | Some from_block, Some to_block
        when BB.start from_block = BB.start to_block ->
          if !verbose then
            printf
              "No fallthroughs in trace with count %Ld from 0x%Lx to \
               0x%Lx:from_block = to_block\n"
              count from_loc.addr to_loc.addr;
          record t from_block ~count
      | Some from_block, Some to_block ->
          if !verbose then
            printf
              "trace (from_addr=0x%Lx,to_addr=0x%Lx)=> \
               (from_linid=%d,to_linid=%d)=> (from_block=%d,to_block=%d)\n"
              from_loc.addr to_loc.addr
              (Option.value_exn from_loc.dbg)
              (Option.value_exn to_loc.dbg)
              (BB.start from_block) (BB.start to_block);
          compute_fallthrough_execounts t (BB.start from_block)
            (BB.start to_block) count
      | _ ->
          if !verbose then
            printf
              "Ignoring trace with count %Ld from 0x%Lx to 0x%Lx:cannot map \
               to_loc or from_loc to cfg blocks.\n"
              count from_loc.addr to_loc.addr;
          mal t count )
  | _ ->
      if !verbose then
        printf
          "Ignoring trace with count %Ld from 0x%Lx to 0x%Lx:to and from \
           function is not the same or not known.\n"
          count from_loc.addr to_loc.addr;
      mal t count

let record_ip t ~loc ~data:count =
  match get_block t loc with
  | None ->
      if !verbose then
        printf "Ignore exec count at 0x%Lx\n, can't map to cfg\n" loc.addr
  | Some block -> record t block ~count
