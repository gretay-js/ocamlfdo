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
(* Create cfg_info *)
open Core
open Loc
open Block_info

let verbose = ref true

(* Map basic blocks of this function to breakdown of execution counts *)
type t = Block_info.t Hashtbl.M(Cfg_label).t [@@deriving sexp]

let terminator_to_string t =
  let open Cfg in
  match t with
  | Return -> "Return"
  | Raise _ -> "Raise"
  | Tailcall _ -> "Tailcall"
  | Branch s -> sprintf "Branch with %d successors" (List.length s)
  | Switch s -> sprintf "Switch with %d successors" (Array.length s)

let mal (func : Func.t) count =
  if !verbose then printf "Malformed trace with %Ld counts.\n" count;
  func.malformed_traces <- Int64.(func.malformed_traces + count)

let first_id (block : Cfg.block) =
  match block.body with
  | [] -> block.terminator.id
  | hd :: _ -> hd.id

let get_block_info t (block : Cfg.block) =
  Hashtbl.find_or_add t block.start ~default:(fun () ->
      let terminator_id =
        match block.terminator.id with
        | 0 ->
            (* use the id of the last instruction in the body *)
            ( match block.terminator.desc with
            | Branch [ (Always, _) ] -> assert true
            | _ -> assert false );
            let last = List.last_exn block.body in
            last.id
        | n -> n
      in
      let first_id = first_id block in
      if !verbose then
        printf
          "make new block info for block.start=%d first_id=%d \
           terminator_id=%d\n"
          block.start first_id terminator_id;
      Block_info.mk ~label:block.start ~first_id ~terminator_id )

let record t block ~count =
  let b = get_block_info t block in
  Block_info.add b ~count

let get_linearid (loc : Loc.t) =
  let dbg = Option.value_exn loc.dbg in
  dbg.line

(* Find basic instruction whose id=[linearid] in [block] *)
let get_basic_instr linearid (block : Cfg.block) =
  List.find block.body ~f:(fun instr -> instr.id = linearid)

(* Find basic instruction right before the one with [linearid] in [block].
   If not found linearid or linearid is the first instruction, then return
   None. *)
exception Found_prev of Cfg.basic Cfg.instruction option

let prev_instr linearid block =
  let open Cfg in
  try
    ignore
      ( List.fold block.body ~init:None ~f:(fun prev instr ->
            if instr.id = linearid then raise (Found_prev prev)
            else Some instr )
        : Cfg.basic Cfg.instruction option );
    None
  with Found_prev prev -> prev

(* Find the block in [cfg] that contains [loc] using its linearid *)
let get_block (loc : Loc.t) cfg =
  match loc.dbg with
  | None ->
      ( if !verbose then
        let rel = Option.value_exn loc.rel in
        printf "No linearid for 0x%Lx in func %d at offsets %d\n" loc.addr
          rel.id rel.offset );
      None
  | Some dbg -> (
    match Cfg_builder.id_to_label cfg dbg.line with
    | None ->
        failwithf "No cfg label for linearid %d in %s" dbg.line dbg.file ()
    | Some label -> (
      match Cfg_builder.get_block cfg label with
      | Some block -> Some block
      | None ->
          failwithf
            "Can't find cfg basic block labeled %d for linearid %d in %s\n"
            label dbg.line dbg.file () ) )

let record_intra t (from_loc : Loc.t) (to_loc : Loc.t) count mispredicts cfg
    =
  let from_block = get_block from_loc cfg in
  let to_block = get_block to_loc cfg in
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
  | Some from_block, Some to_block -> (
      record t from_block ~count;
      record t to_block ~count;
      let from_linearid = get_linearid from_loc in
      let to_linearid = get_linearid to_loc in
      let to_block_first_id = first_id to_block in
      assert (to_block_first_id = to_linearid);
      let bi = get_block_info t from_block in
      let b =
        { Block_info.target = Some to_loc;
          target_label = Some to_block.start;
          target_id = Some to_block_first_id;
          intra = true;
          fallthrough = false;
          taken = count;
          mispredicts
        }
      in
      if from_block.terminator.id = from_linearid then (
        (* Find the corresponding successor *)
        match from_block.terminator.desc with
        | Return
        (* return from a recursive call *)
        (* target must be right after a call *)
        | Tailcall _ ->
            (* tailcall *)
            if !verbose then
              printf "Tailcall from linid=%d from_label=%d %Ld"
                from_linearid from_block.start count
        | Raise _ ->
            (* target must be a hanlder block *)
            assert (Cfg_builder.is_trap_handler cfg to_block.start)
        | Branch _ | Switch _ ->
            let successors = Cfg.successor_labels from_block in
            assert (List.mem successors to_block.start ~equal:Int.equal);
            Block_info.add_branch bi b )
      else
        (* recursive call, find the call instruction *)
        let instr =
          Option.value_exn (get_basic_instr from_linearid from_block)
        in
        match instr.desc with
        | Cfg.Call _ -> add_call bi ~callsite:from_loc ~callee:b
        | _ -> assert false )

let record_exit t (from_loc : Loc.t) (to_loc : Loc.t) count mispredicts cfg
    =
  (* Branch going outside of this function. *)
  match get_block from_loc cfg with
  | None ->
      if !verbose then
        printf
          "Ignore inter branch count %Ld from 0x%Lx. Can't map to CFG.\n"
          count from_loc.addr
  | Some from_block -> (
      record t from_block ~count;
      (* Find the corresponding instruction and update its counters. The
         instruction is either a terminator or a call.*)
      let linearid = get_linearid from_loc in
      let terminator = from_block.terminator in
      let bi = get_block_info t from_block in
      if terminator.id = linearid then
        (* terminator *)
        match terminator.desc with
        | Branch _ | Switch _ | Tailcall _ ->
            (* can't branch outside the current function *)
            assert false
        | Return | Raise _ ->
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
      else
        (* Call *)
        match get_basic_instr linearid from_block with
        | None -> assert false
        (* we've checked before for presence of dbg info *)
        | Some instr -> (
          match instr.desc with
          | Call _ ->
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
              Block_info.add_call bi ~callsite:from_loc ~callee:b
          | _ -> assert false ) )

let record_entry t (from_loc : Loc.t) (to_loc : Loc.t) count _mispredicts
    cfg =
  (* Branch into this function from another function, which may be unknown.
     One of the following situations: Callee: branch target is the first
     instr in the entry block. Return from call: branch target is a label
     after the call site. Exception handler: branch target is a trap
     handler. *)
  if from_loc.addr < 0L then (
    if
      (* [from_loc.addr] of this branch does not belong to the binary and
         usually to_addr cannot be target of jump because it is not a return
         from a call instruction and not an entry function. Could it be a
         context switch? *)
      !verbose
    then
      printf "record_entry at 0x%Lx -> 0x%Lx (from_addr < 0)\n"
        from_loc.addr to_loc.addr )
  else
    match get_block to_loc cfg with
    | None ->
        if !verbose then
          printf
            "Ignore inter branch count %Ld to 0x%Lx. Can't map to CFG.\n"
            count to_loc.addr
    | Some to_block -> (
        record t to_block ~count;
        (* Find the corresponding instruction and update its counters.*)
        let linearid = get_linearid to_loc in
        let _bi = get_block_info t to_block in
        let first_instr_linearid =
          match to_block.body with
          | [] -> to_block.terminator.id
          | hd :: _ -> hd.id
        in
        (* We could infer call targets from this info, but its not
           implemented yet because we don't have much use for it and it
           would change the way Block_info.add_call maps callsites using
           locations because we don't necessarily have a location for the
           caller. Also note that add_call currently checks that each callee
           can be installed at most once, as guaranteed by the aggregated
           profile. *)
        if
          first_instr_linearid = linearid
          && ( to_block.start = Cfg_builder.entry_label cfg
             (* Callee *)
             || Cfg_builder.is_trap_handler cfg to_block.start
                (* Exception handler *) )
        then ( (* Block_info of potential callers can be updated *) )
        else
          (* Return from a call. Find predecessor instruction and check that
             it is a call. There could be more than one predecessor. We have
             enough information to find which one in some cases. *)
          let record_call (instr : Cfg.basic Cfg.instruction) =
            match instr.desc with
            | Call _ -> ()
            | _ ->
                if !verbose then
                  printf
                    "record_entry failed at 0x%Lx -> 0x%Lx linid=%d \
                     unrecognized as call instr.id=%d\n"
                    from_loc.addr to_loc.addr linearid instr.id;
                assert false
          in
          let find_prev_block_call () =
            (* find a predecessor block that must end in a call and
               fallthrough. *)
            Cfg.LabelSet.iter
              (fun pred_label ->
                let pred =
                  Option.value_exn (Cfg_builder.get_block cfg pred_label)
                in
                match pred.terminator.desc with
                | Branch [ (Always, label) ] when label = to_block.start ->
                    let last_instr = List.last_exn to_block.body in
                    record_call last_instr
                | _ ->
                    if !verbose then
                      printf
                        "record_entry failed at 0x%Lx -> 0x%Lx \
                         to_first_instr_linid=%d to_linearid=%d%s\n"
                        from_loc.addr to_loc.addr first_instr_linearid
                        linearid
                        (terminator_to_string pred.terminator.desc) )
              to_block.predecessors
          in
          if first_instr_linearid = linearid then find_prev_block_call ()
          else
            match prev_instr linearid to_block with
            | Some instr -> record_call instr
            | None -> (
                (* instr with linearid must be the terminator *)
                assert (to_block.terminator.id = linearid);
                match to_block.body with
                | [] ->
                    (* empty body means the call was in one of the pred
                       blocks *)
                    find_prev_block_call ()
                | _ ->
                    let last_instr = List.last_exn to_block.body in
                    record_call last_instr ) )

(* Depending on the settings of perf record and the corresponding CPU
   configuration, LBR may capture different kinds of branches, including
   function calls and returns. *)
let record_branch t from_loc to_loc count mispredicts (func : Func.t) cfg =
  let open Loc in
  (* at least one of the locations is known to be in this function *)
  match (from_loc.rel, to_loc.rel) with
  | Some from_rel, Some to_rel
    when from_rel.id = func.id && to_rel.id = func.id ->
      record_intra t from_loc to_loc count mispredicts cfg
  | Some from_rel, _ when from_rel.id = func.id ->
      record_exit t from_loc to_loc count mispredicts cfg
  | _, Some to_rel when to_rel.id = func.id ->
      record_entry t from_loc to_loc count mispredicts cfg
  | _ -> assert false

exception Malformed_fallthrough_trace

let compute_fallthrough_execounts t from_lbl to_lbl count (func : Func.t)
    cfg =
  (* Get the part of the layout starting from from_block up to but not
     including to_block *)
  try
    let fallthrough =
      Cfg_builder.get_layout cfg
      |> List.drop_while ~f:(fun lbl -> not (lbl = from_lbl))
    in
    let fallthrough =
      match List.findi fallthrough ~f:(fun _ lbl -> lbl = to_lbl) with
      | None -> raise Malformed_fallthrough_trace
      | Some (to_pos, _) -> List.take fallthrough to_pos
    in
    if !verbose then (
      printf "%s trace (from_lbl=%d,to_lbl=%d)\n fallthrough: " func.name
        from_lbl to_lbl;
      List.iter fallthrough ~f:(fun lbl -> printf " %d" lbl);
      printf "\nlayout=";
      List.iter (Cfg_builder.get_layout cfg) ~f:(fun lbl -> printf " %d" lbl);
      printf "\n" );
    (* Check that all terminators fall through *)
    let check_fallthrough src_lbl dst_lbl =
      let block = Option.value_exn (Cfg_builder.get_block cfg src_lbl) in
      match block.terminator.desc with
      | Branch _ | Switch _ ->
          if !verbose then (
            printf
              "check_fallthrough in %s trace (from_lbl=%d,to_lbl=%d): \
               src_lbl=%d dst_lbl=%d\n\
               src_block.successor_labels:\n"
              func.name from_lbl to_lbl src_lbl dst_lbl;
            List.iter (Cfg.successor_labels block) ~f:(fun lbl ->
                printf "%d\n" lbl ) );
          if List.mem (Cfg.successor_labels block) dst_lbl ~equal:Int.equal
          then src_lbl
          else (
            if !verbose then
              printf
                "Malformed fallthrough in %s trace \
                 (from_lbl=%d,to_lbl=%d): src_lbl=%d dst_lbl=%d\n\
                 src_block.successor_labels:\n"
                func.name from_lbl to_lbl src_lbl dst_lbl;
            raise Malformed_fallthrough_trace )
      | _ ->
          if !verbose then
            printf
              "Unexpected terminator %s in fallthrough trace \
               %s(from_lbl=%d,to_lbl=%d): src_lbl=%d dst_lbl=%d\n\
               src_block.successor_labels:\n"
              (terminator_to_string block.terminator.desc)
              func.name from_lbl to_lbl src_lbl dst_lbl;
          raise Malformed_fallthrough_trace
    in
    assert (
      from_lbl
      = List.fold_right fallthrough ~init:to_lbl ~f:check_fallthrough );
    (* Account only for the intermediate blocks in the trace. Endpoint
       blocks of the trace are accounted for when we handled their LBR
       branches. *)
    let record_fallthrough src_lbl dst_lbl =
      if !verbose then
        printf "record_fallthrough %d->%d count=%Ld\n" src_lbl dst_lbl count;
      let dst_block =
        Option.value_exn (Cfg_builder.get_block cfg dst_lbl)
      in
      record t dst_block ~count;
      let target_bi = get_block_info t dst_block in
      let src_block =
        Option.value_exn (Cfg_builder.get_block cfg src_lbl)
      in
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
    if !verbose then (
      printf "recorded healthy trace from %d to %d count %Ld\n" from_lbl
        to_lbl count;
      printf !"%{sexp:t}\n" t )
  with Malformed_fallthrough_trace ->
    (* If the trace is malformed, don't add counts *)
    mal func count

let record_trace t from_loc to_loc count (func : Func.t) cfg =
  (* both locations must be in this function *)
  let open Loc in
  match (from_loc.rel, to_loc.rel) with
  | Some from_rel, Some to_rel
    when from_rel.id = func.id && to_rel.id = func.id -> (
    match (get_block from_loc cfg, get_block to_loc cfg) with
    | Some from_block, Some to_block when from_block.start = to_block.start
      ->
        if !verbose then
          if from_block.start = to_block.start then
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
            (let from_dbg = Option.value_exn from_loc.dbg in
             from_dbg.line)
            (let to_dbg = Option.value_exn to_loc.dbg in
             to_dbg.line)
            from_block.start to_block.start;
        compute_fallthrough_execounts t from_block.start to_block.start
          count func cfg
    | _ ->
        if !verbose then
          printf
            "Ignoring trace with count %Ld from 0x%Lx to 0x%Lx:cannot map \
             to_loc or from_loc to cfg blocks.\n"
            count from_loc.addr to_loc.addr;
        mal func count )
  | _ ->
      if !verbose then
        printf
          "Ignoring trace with count %Ld from 0x%Lx to 0x%Lx:to and from \
           function is not the same or not known.\n"
          count from_loc.addr to_loc.addr;
      mal func count

let create () = Hashtbl.create (module Cfg_label)
