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

let verbose = ref true

module Loc = struct
  (* Dwarf info associated with a location *)
  type dbg = {
    file : string;  (* filename *)
    line : int;     (* line number *)
  } [@@deriving compare, sexp, hash]

  type rel = {
    id : int;       (* Unique id of the containing function symbol *)
    offset : int;   (* Offset from the start of the function *)
  } [@@deriving compare, sexp, hash]

  type t = {
    addr : int64;     (* Raw address in the original binary *)
    rel : rel option; (* Containing function info and relative offset *)
    dbg : dbg option;
  } [@@deriving compare, sexp, hash]

  let to_bolt_string t =
    match t.func with
    | None -> "0 [unknown] 0"
    | Some func -> sprintf "1 %s %x" func.name func.offset

end

module Aggregated_perf = struct

    module P = struct
      type t = int64 * int64 [@@deriving compare, hash, sexp]
    end
    module ST = int64 Hashtbl.M(Int64); (* Single address counter Table *)
    module PT = int64 Hashtbl.M(P); (* Pair of addresses counter Table *)
    type t = {
      instructions : ST.t;
      branches : PT.t;
      (* execution count: number of times the branch was taken. *)
      mispredicts : PT.t;
      (* number of times the branch was mispredicted:
         branch target mispredicted or
         branch direction was mispredicted. *)
      traces : PT.t;
      (* execution count: number of times the trace was taken. *)
    } [@@deriving sexp]

    let empty =
      { instructions = Hashtbl.create (module Int64);
        branches = Hashtbl.create (module P);
        mispredicts = Hashtbl.create (module P);
        traces = Hashtbl.create (module P);
      }

    let read filename =
      if !verbose then
        printf "Reading aggregated perf profile from %s\n" filename;
      let t =
        match Parsexp_io.load (module Parsexp.Single) ~filename with
        | Ok t_sexp -> t_of_sexp t_sexp
        | Error error ->
          Parsexp.Parse_error.report Caml.Format.std_formatter error ~filename;
          failwith "Cannot parse aggregated profile file"
      in
      if !verbose then begin
        Printf.printf !"Aggregated perf profile:\n%{sexp:t}\n" t;
      end;
      t

    let write t filename =
      if !verbose then
        printf "Writing aggregated perf profile to %s\n" filename;
      let chan = Out_channel.create filename in
      Printf.fprintf chan !"%{sexp:t}\n" t;
      Out_channel.close chan
end

module Execount : Cfg.User_data = struct
  type t = int64;
  type branch  = {
    taken : t;
    mispredict : t;
  }
  type target = Loc.t

  let add d1 d2 =
    let d1 = Option.value d1 ~default:0L in
    let d2 = Option.value d2 ~default:0L in
    Some Int64.(d1 + d2)

  module Func_data = struct type t = t end
  module Block_data = struct type t = t end
  module Instr_data = struct type t = t end
  module Succ_data = struct type t = branch end
  module Call_data = struct
    type t =
      | Direct of branch
      | Indirect of (target * branch) list
  end
end

module Func = struct
  type t = {
    id : int;       (* Unique identifier we assign to this function *)
    name : string;  (* Name of the function symbol *)
    start : int;    (* Raw start address of the function inthe original binary *)
    mutable cfg : Cfg.Make(module Execount).t option;
    (* Control flow graph annotated with execution counters *)
    (* CR gyorsh: this might grow too large, whole program cfgs
       collected through the entire compilation. *)
    mutable count : Execount.t;  (* Preliminary execution count *)
  }

  (* descending order of execution counts (reverse order of compare) *)
  (* Tie breaker using name.
     Slower than id but more stable w.r.t. changes in perf data and ocamlfdo,
     because ids are an artifact of the way ocamlfdo reads
     and decodes locations.
     Change to tie breaker using id if speed becomes a problem. *)
  let compare f1 f2 =
    let res = Int.compare f2.count f1.count in
    if res = 0 then String.compare f1.name f2.name
    else res)

end

module Aggregated_decoded = struct
  open Func
  type t = {
    aggregated_perf : Aggregated_perf.t;
    addr2loc : Loc.t Hashtbl.M(Int64).t; (* map raw addresses to locations *)
    name2id : int Hashtbl.M(String).t; (* map func name to func id *)
    functions : Func.t Hashtbl.M(int).t; (* map func id to func info *)
  }  [@@deriving sexp]

  let mk a size =
    { aggregated_perf=a;
      addr2loc = Hashtbl.create ~size (module Int64);
      name2id = Hashtbl.create (module String);
      functions = Hashtbl.create (module Int);
    } in

  (* Find all locs that mention this function.
     Translate linear ids of these locations to cfg labels.
     Get the corresponding basic blocks and update their execounts. *)
  (* CR gyorsh: this might be too slow, because for each function it needs
     to go over all code addresses in the original binary, in the worst case.
     Alternatively, we can collect per function all locations that mention
     it when we are constructing addr2loc and functions.
     However, for branches and traces tables, where key is a pair of
     addresses, we need to search for pairs where possibly only the second
     element (destination) is in the current function.
     This will be as slow as unless we change the representation of
     branches and traces or store reverse keys in rev_branches.
     Alternatively, store all locations and pairs of locations
     where the function is mentioned.
  *)

  let compute_execounts t func cfg_builder =
    let cfg = Cfg_builder.get_cfg cfg_builder in
    let a = t.aggregated_perf in
    let get_block loc =
      assert (loc.rel.id = func.id);
      match Cfg_builder.id_to_label cfg_builder loc.line with
      | None ->
        if verbose then
          printf "Execution count for linearid %d ignored, \
                  can't map to cfg node\n" loc.line;
      | Some label ->
        Hashtbl.find cfg.blocks label
    in
    List.iter func.locs ~f:(fun loc ->
      let block = get_block
    )
    Hashtbl.iteri a.instructions ~f:(fun ~key ~data ->
      let loc = Hashtbl.find_exn t.addr2loc key in
      if loc.rel.id = func.id then
        match Cfg_builder.id_to_label cfg_builder loc.line with
        | None ->
          if verbose then
            printf "Execution count for linearid %d ignored, \
                    can't map to cfg node\n" loc.line;
          ()
        | Some label ->
          let block = Hashtbl.find cfg.blocks label in
          block.data <- Execount.add block.data data
    );
    Hashtbl.iteri a.branches ~f:(fun ~key ~data ->
      let (f,t) = key in
      let loc = Hashtbl.find_exn t.addr2loc key in
      if loc.rel.id = func.id then
        match Cfg_builder.id_to_label cfg_builder loc.line with
        | None ->
          if verbose then
            printf "Execution count for linearid %d ignored, \
                    can't map to cfg node\n" loc.line;
          ()
        | Some label ->
          let block = Hashtbl.find cfg.blocks label in
          block.data <- Execount.add block.data data
    );
    let entry_block = Hashtbl.find cfg.blocks cfg.entry_label in
    cfg.data <- Execount.add cfg.data cfg.entry_block.data;


  (* let compute_execounts t func cfg_builder =
   *   let cfg = Cfg_builder.get_cfg cfg_builder in
   *   let a = t.aggregated_perf in
   *   let get_block addr =
   *     let loc = Hashtbl.find_exn t.addr2loc addr in
   *     if loc.rel.id = func.id then
   *       match Cfg_builder.id_to_label cfg_builder loc.line with
   *       | None ->
   *         if verbose then
   *           printf "Execution count for linearid %d ignored, \
   *                   can't map to cfg node\n" loc.line;
   *         ()
   *       | Some label ->
   *         let block = Hashtbl.find cfg.blocks label in
   *   in
   *   Hashtbl.iteri a.instructions ~f:(fun ~key ~data ->
   *     let loc = Hashtbl.find_exn t.addr2loc key in
   *     if loc.rel.id = func.id then
   *       match Cfg_builder.id_to_label cfg_builder loc.line with
   *       | None ->
   *         if verbose then
   *           printf "Execution count for linearid %d ignored, \
   *                   can't map to cfg node\n" loc.line;
   *         ()
   *       | Some label ->
   *         let block = Hashtbl.find cfg.blocks label in
   *         block.data <- Execount.add block.data data
   *   );
   *   Hashtbl.iteri a.branches ~f:(fun ~key ~data ->
   *     let (f,t) = key in
   *     let loc = Hashtbl.find_exn t.addr2loc key in
   *     if loc.rel.id = func.id then
   *       match Cfg_builder.id_to_label cfg_builder loc.line with
   *       | None ->
   *         if verbose then
   *           printf "Execution count for linearid %d ignored, \
   *                   can't map to cfg node\n" loc.line;
   *         ()
   *       | Some label ->
   *         let block = Hashtbl.find cfg.blocks label in
   *         block.data <- Execount.add block.data data
   *   );
   *   let entry_block = Hashtbl.find cfg.blocks cfg.entry_label in
   *   cfg.data <- Execount.add cfg.data cfg.entry_block.data; *)

  (* Compute execution counts from CFG *)
  let compute_cfg_execounts t name cfg_builder =
    let id = Hashtbl.find_exn t.name2id name in
    match Hashtbl.find t.functions id with
    | None ->
      if verbose then
        printf "Not found profile for %s with cfg.\n" name;
    | Some id ->
      let func = Hashtbl.find_exn t.functions id in
      if func.count > 0 then
        compute_execounts t func cfg_builder


  (* Execution count of a function is determined from the execution counts
     of samples contained in this function. It uses LBR info:
     if a branch source or target is contained in the function,
     it contributes to execution count of the function.
     Does not use the CFG.
     In particular, it does not count instructions that can be traced using LBR.
     The advantage is that we can compute it for non-OCaml functions. *)
  let compute_func_execounts t =
    let a = t.aggregated_perf in
    let len = Hashtbl.length t.functions in
    let func_execount = Array.create ~len 0L in
    let add addr count =
      let loc = Hashtbl.find_exn t.addr2loc addr in
      let id = loc.rel.id in
      func_execount.(id) <- Int64.(func_execount.(id) + 1L) in
    (* Iterate over instruction and brances, but not traces. *)
    Hash.iteri a.instructions
      ~f:(fun ~key ~data -> add key add);
    Hash.iteri a.branches
      ~f:(fun ~key ~data -> let (f,t) = key in add f data; add t data);
    if verbose then
      Array.iteri t.func_execount
        ~f(fun i c ->
          let func = Hashtbl.find_exn functions i in
          printf "(%d) %s at 0x%Lx: %L" func.id func.name func.start c);
    Hashtbl.iter t.functions
      ~f:(fun func ->
        func.count <- func_execount.(func.id))

  (* Find or add the function and return its id *)
  let get_func_id t ~name ~start =
    match Hashtbl.find t.name2id name with
    | None ->
      let id = Hashtbl.length functions in
      let func = { Func.
        id;
        name;
        start;
        cfg=None;
        counts=0L;
      } in
      Hashtbl.add_exn t.functions ~key:id ~data:func
      Hashtbl.add_exn t.name2id ~key:name ~data:id;
    | Some id ->
      let func = Hashtbl.find_exn t.functions id in
      assert (func.id = id);
      assert (func.name = name);
      assert (func.start = start);
      func.id

  let decode_loc t locations addr =
    let open Loc in
    match Elf_locations.resolve_function_containing locations
            ~program_counter:addr with
    | None ->
      if !verbose then
        printf "Cannot find function symbol containing 0x%Lx\n" addr;
      { addr; func=None; dbg=None; }
    | Some interval ->
      let name = interval.v in
      let start = interval.l in
      let offset =
        match Int64.(to_int (addr - start)) with
        | None -> failwithf "Offset too big: 0x%Lx" (Int64.(addr - start)) ()
        | Some offset -> assert (offset >= 0); offset in
      let id = get_func_id t name start in
      let func = Some { id; offset; } in
      let dbg =
        match Ocaml_locations.(decode_line locations
                                 ~program_counter:addr name Linearid) with
        | None -> None
        | Some (file,line) -> Some {Loc.file;line;} in
      { addr; func; dbg; }

  let decode_aggregated locations aggregated_perf =
    if !verbose then
      printf "Decoding perf profile.\n";

    (* Collect all addresses that need decoding. *)
    (* Mispredicts and traces use the same addresses as branches,
       so no need to add them *)
    let len = Hashtbl.length aggregated_perf.instructions +
              Hashtbl.length aggregated_perf.branches in
    (* Elf_locations does not use Core, so we need to create Caml.Hashtbl *)
    let addresses = Caml.Hashtbl.create len in
    let add key =
      if not (Caml.Hashtbl.mem addresses key) then
        Caml.Hashtbl.add addresses key ();
    in
    let add2 (f,t) = add f; add t in
    Hashtbl.iter_keys t.instructions ~f:add;
    Hashtbl.iter_keys t.branches ~f:add2;
    (* A key may appear in both t.instruction and t.branches *)
    assert (size <= len);
    let size = Caml.Hashtbl.length addresses in
    (* Resolve and cache all addresses we need in one pass over the binary. *)
    Elf_locations.resolve_all locations addresses ~reset:true;
    (* Decode all locations: map addresses to locations *)
    let t = mk aggregated_perf size in
    Caml.Hashtbl.iter
      (fun addr _ ->
         let loc = decode_loc t locations addr in
         Hashtbl.add_exn t.addr2loc ~key:addr ~data:loc)
      addresses;
    compute_func_execounts t;
    t

  let read filename =
    if !verbose then
      printf "Reading aggregated decoded profile from %s\n" filename;
    let t =
      match Parsexp_io.load (module Parsexp.Single) ~filename with
      | Ok t_sexp -> t_of_sexp t_sexp
      | Error error ->
        Parsexp.Parse_error.report Caml.Format.std_formatter error ~filename;
        failwith "Cannot parse aggregated decoded profile file"
    in
    if !verbose then begin
      Printf.printf !"Aggregated decoded profile:\n%{sexp:t}\n" t;
    end;
    t

  let write t filename =
    if !verbose then
      printf "Writing aggregated decoded profile to %s\n" filename;
    let chan = Out_channel.create filename in
    Printf.fprintf chan !"%{sexp:t}\n" t;
    Out_channel.close chan

end

type t = Aggregated_decoded.t

let write_top_functions t filename =
  if !verbose then
    printf "Writing top functions to %s\n" filename;
  let open Aggregated_decoded in
  (* Sort functions using preliminary function-level execution counts
     in descending order. *)
  let sorted = List.sort (Hashtbl.data t.functions) ~compare:Func.compare in
  let fl = List.map sorted ~f:(fun func -> Func.func.id) in
  Func_layout.write_linker_script fl filename

let write_bolt t filename =
  let open Loc in
  let open Aggregated_perf in
  let open Aggregated_decoded in
  if !verbose then
    printf "Writing aggregated decoded prfile in bolt form to %s\n" filename;
  let chan = Out_channel.create filename in
  let write_bolt_count ~key ~data =
      let mis = Option.value
                  (Hashtbl.find t.counts.mispredicts key)
                  ~default:0L in
      let (from_addr, to_addr) = key in
      let from_loc = loc t from_addr  in
      let to_loc = loc t to_addr in
      match from_loc.func, to_loc.func with
      | None, None -> ()
      | _ ->
        Printf.fprintf chan "%s %s %Ld %Ld\n"
          (to_bolt_string from_loc)
          (to_bolt_string to_loc)
          mis
          data in
  Hashtbl.iteri t.counts.branches ~f:write_bolt_count;
  Out_channel.close chan

let compute_execounts t name cfg =
  Aggregated_decoded.compute_execounts t name cfg

module Perf = struct
  (* Perf profile format as output of perf script -F pid,ip,brstack *)

  type mispredict_flag = M | P | NOT_SUPPORTED
  [@@deriving compare, sexp]

  let mispredicted = function
    | M -> true
    | _ -> false

  type br = {
    from_addr : int64;
    to_addr : int64;
    mispredict : mispredict_flag;
    index : int; (* Position on the stack, with 0 being the most recent branch.
                    This field is only used for only for validation. *)
    (* cycles : int; *)
  } [@@deriving compare, sexp]

  type sample = {
    ip: int64;  (* instruction pointer where the sample was taken *)
    brstack: br list; (* branch stack is the last branch record (LBR) *)
  } [@@deriving compare, sexp]

  type t = sample list
  [@@deriving compare, sexp]

  let parse_br index s =
    match String.split s ~on:'/' with
    | [from_addr;to_addr;m;t;a;c] ->
      let mispredict = match m with
        | "M" -> M
        | "P" -> P
        | "-" -> NOT_SUPPORTED
        | _ -> failwithf "Cannot parse mispredict flag %s in %s" m s ()
      in
      (* Parse and ignore t and a flags. *)
      begin match t with
      | "X" | "-" -> ()
      | _ -> failwithf "Cannot parse mispredict flag %s in %s" m s ()
      end;
      begin match a with
      | "A" | "-" -> ()
      | _ -> failwithf "Cannot parse mispredict flag %s in %s" a s ()
      end;
      (* Parse and ignore cycles. CR gyorsh: use for optimizations. *)
      let _cycles = Int.of_string c in
      {
        from_addr = Int64.of_string from_addr;
        to_addr = Int64.of_string to_addr;
        index;
        mispredict;
      }
    | _ -> failwithf "Cannot parse %s\n" s ()

  let print t =
    Printf.printf !"%{sexp:t}\n" t

  (* The most recent branch is printed first by perf script.
     The number of branch entries vary based on the underlying hardware.
     This function reverses the stack from its perf profile order. *)
  let rec parse_brstack (index,brstack) row =
    match row with
    | [] -> (index,brstack)
    | hd::tl ->
      let brstack = (parse_br index hd)::brstack
      in parse_brstack (index+1,brstack) tl

  let split_on_whitespace row =
    let r = String.split ~on:' ' row in
    List.filter r ~f:(fun s -> not (String.is_empty s))

  let hex s =
    if String.is_prefix ~prefix:"0x" s then
      s
    else
      "0x" ^ s

  let row_to_sample ~keep_pid row =
    match split_on_whitespace row with
    | pid::ip::rest ->
      let pid = Int.of_string pid in
      if keep_pid pid then begin
        if !verbose then
          printf "parsing ip %s\n" ip;
        let sample =  {
          ip = Int64.of_string (hex ip);
          brstack = snd (parse_brstack (0,[]) rest);
        } in
        Some sample
      end
      else
        None
    | _ -> failwithf "Cannot parse %s\n" row ()

  let pids = ref Int.Set.empty

  let check_keep_pid ?expected_pid p =
    if !verbose then begin
      if not (Int.Set.mem !pids p) then begin
        printf "Found new pid: %d\n" p;
        pids := Int.Set.add !pids p;
      end
    end;
    match expected_pid with
    | None -> true
    | Some expected_pid ->
      if expected_pid = p then true
      else begin
        if !verbose then
          printf "Mismatch pid: expected %L found %L\n" expected_pid p;
        false
      end

  let read ?(expected_pid=None) filename =
    if !verbose then
      printf "Reading perf profile generated by \
              \"perf script -F pid,ip,brstack\" from %s\n" filename;
    let chan = In_channel.create filename in
    let keep_pid = check_keep_pid ?expected_pid in
    let t = In_channel.fold_lines chan ~init:[]
              ~f:(fun acc row ->
                match row_to_sample ~keep_pid row with
                | None -> acc
                | Some sample -> sample::acc) in
    In_channel.close chan;
    if !verbose then begin
      print t;
      Printf.printf "Found pids:\n";
      Int.Set.iter !pids ~f:(fun pid -> printf "%d\n" pid)
    end;
    t


  type stats = {
    ignored:int;
    total:int;
    lbr:int;
  }
  let read_and_aggregate ?(expected_pid=None) filename =
    if !verbose then
      printf "Aggregate perf profile from %s.\n" filename;

    let inc table key =
      Hashtbl.update table key
        ~f:(fun v -> Int64.(1L + Option.value ~default:0L v)) in
    let aggregated = Aggregated_perf.empty in
    (* CR gyorsh: aggregate during parsing of perf profile *)
    let aggregate sample =
      inc aggregated.instructions sample.ip;
      List.iter sample.brstack
        ~f:(fun br ->
          let key = (br.from_addr,br.to_addr) in
          inc aggregated.branches key;
          if mispredicted br.mispredict then
            inc aggregated.mispredicts key;
        );
      (* Instructions executed between branches can be inferred from brstack *)
      ignore (List.fold sample.brstack ~init:None
                ~f:(fun prev cur ->
                  match prev with
                  | None -> Some cur
                  | Some prev ->
                    assert (prev.index = (cur.index + 1));
                    let from_addr = prev.to_addr in
                    let to_addr = cur.from_addr in
                    let key = (from_addr,to_addr) in
                    inc aggregated.traces key;
                    Some cur
                ): br option)

    in
    let chan = In_channel.create filename in
    let keep_pid = check_keep_pid ?expected_pid in
    let empty_stats = {ignored=0;total=0;lbr=0} in
    let stats = In_channel.fold_lines chan ~init:empty_stats
      ~f:(fun stats row ->
        match row_to_sample ~keep_pid row with
        | None ->
          { stats with ignored=stats.ignored+1; total=stats.total+1 }
        | Some sample ->
          aggregate sample;
          { stats with total=stats.total+1;
                       lbr=stats.lbr+(List.length sample.brstack) }
         ) in
    In_channel.close chan;
    if !verbose then begin
      Printf.printf "Read %d samples with %d LBR entries\n" stats.total stats.lbr;
      let r = Float.( (of_int stats.ignored) * 100.0 / (of_int stats.total) ) in
      Printf.printf "Ignored %d samples (%.1f)\n" stats.ignored r;
      Printf.printf "Found pids:\n";
      Int.Set.iter !pids ~f:(fun pid -> printf "%d\n" pid)
    end;
      aggregated

end

