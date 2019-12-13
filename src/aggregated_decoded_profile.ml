open Core
open Dbg
open Loc
open Func
module Cfg_with_layout = Ocamlcfg.Cfg_with_layout
module Cfg = Ocamlcfg.Cfg

let verbose = ref true

type t =
  { (* map raw addresses to locations *)
    addr2loc : Loc.t Hashtbl.M(Addr).t;
    (* map func name to func id *)
    name2id : int Hashtbl.M(String).t;
    (* map func id to func info *)
    functions : Func.t Hashtbl.M(Int).t;
    (* map func id to cfg_info of that function. *)
    (* sparse, only holds functions that have cfg *and* execounts. *)
    (* logically it should be defined inside Func.t but it creates a cyclic
       dependency between . The advantage of the current design is smaller
       space that a Func takes if it doesn't have a cfg_info *)
    execounts : Cfg_info.blocks Hashtbl.M(Int).t;
    (* map name of compilation unit or function to its md5 digest. Currently
       contains only crcs of linear IR. Not using Caml.Digest.t because it
       does not have sexp. Not using Core's Digest because digests generated
       by the compiler using Caml.Digest might disagree. *)
    crcs : Crcs.tbl;
    (* buildid of the executable, if known *)
    mutable buildid : string option
  }
[@@deriving sexp]

let mk size crcs buildid =
  { addr2loc = Hashtbl.create ~size (module Addr);
    name2id = Hashtbl.create (module String);
    functions = Hashtbl.create (module Int);
    execounts = Hashtbl.create (module Int);
    crcs;
    buildid
  }

let get_func t addr =
  match Hashtbl.find t.addr2loc addr with
  | None ->
      printf "Not found any cached location for address 0x%Lx\n" addr;
      assert false
  | Some loc -> (
      match loc.rel with
      | None -> None
      | Some rel ->
          let id = rel.id in
          let func = Hashtbl.find_exn t.functions id in
          Some func )

(* Partition aggregated_perf to functions and calculate total execution
   counts of each function. Total execution count of a function is determined
   from the execution counts of samples contained in this function. It uses
   LBR info: if a branch source or target is contained in the function, it
   contributes to execution count of the function. It does not use the CFG.
   In particular, it does not count instructions that can be traced using
   LBR. The advantage is that we can compute it for non-OCaml functions. *)
let create_func_execounts t (agg : Aggregated_perf_profile.t) =
  Hashtbl.iteri agg.instructions ~f:(fun ~key ~data ->
      match get_func t key with
      | None -> ()
      | Some func ->
          func.count <- Int64.(func.count + data);
          Hashtbl.add_exn func.agg.instructions ~key ~data);
  let process (from_addr, to_addr) update =
    match (get_func t from_addr, get_func t to_addr) with
    | None, None -> ()
    | None, Some to_func -> update to_func
    | Some from_func, None -> update from_func
    | Some from_func, Some to_func ->
        if from_func.id = to_func.id then update to_func
        else (
          (* interprocedural branch: add to both functions *)
          update from_func;
          update to_func )
  in
  Hashtbl.iteri agg.branches ~f:(fun ~key ~data ->
      let mispredicts =
        Option.value (Hashtbl.find agg.mispredicts key) ~default:0L
      in
      let update_br func =
        func.count <- Int64.(func.count + data);
        Hashtbl.add_exn func.agg.branches ~key ~data;
        if Int64.(mispredicts > 0L) then
          Hashtbl.add_exn func.agg.mispredicts ~key ~data:mispredicts
      in
      process key update_br);
  Hashtbl.iteri agg.traces ~f:(fun ~key ~data ->
      (* traces don't contribute to func's total count because it is account
         for in branches. *)
      let update_tr func = Hashtbl.add_exn func.agg.traces ~key ~data in
      process key update_tr)

(* Find or add the function and return its id *)
let get_func_id t ~name ~start ~finish =
  match Hashtbl.find t.name2id name with
  | None ->
      let id = Hashtbl.length t.functions in
      let func = Func.mk ~id ~name ~start ~finish in
      Hashtbl.add_exn t.functions ~key:id ~data:func;
      Hashtbl.add_exn t.name2id ~key:name ~data:id;
      func.id
  | Some id ->
      let func = Hashtbl.find_exn t.functions id in
      assert (func.id = id);
      assert (String.equal func.name name);
      assert (Addr.equal func.start start);
      assert (Addr.equal func.finish finish);
      func.id

let decode_addr t addr interval dbg =
  let open Loc in
  let loc =
    match interval with
    | None ->
        if !verbose then
          printf "Cannot find function symbol containing 0x%Lx\n" addr;
        { addr; rel = None; dbg = None }
    | Some interval ->
        let open Intervals in
        let name = interval.v in
        let start = interval.l in
        let finish = interval.r in
        let offset =
          match Int64.(to_int (addr - start)) with
          | None ->
              Report.user_error "Offset too big: 0x%Lx"
                Addr.(addr - start)
                ()
          | Some offset ->
              assert (offset >= 0);
              offset
        in
        let id = get_func_id t ~name ~start ~finish in
        let rel = Some { id; offset; label = None } in
        let dbg =
          match dbg with
          | None ->
              if !verbose then
                Printf.printf "Elf location NOT FOUND at 0x%Lx\n" addr;
              None
          | Some dbg ->
              if !verbose then
                Printf.printf "%Lx:%Lx:%s:%d\n" addr start dbg.file dbg.line;

              (* Check that the filename has supported suffix and return it. *)
              if Filenames.(compare Linear ~expected:name ~actual:dbg.file)
              then (
                (* Set has_linearids of this function *)
                let func = Hashtbl.find_exn t.functions id in
                func.has_linearids <- true;
                Some dbg )
              else None
        in
        if !verbose then printf "addr2loc adding addr=0x%Lx\n" addr;
        { addr; rel; dbg }
  in
  Hashtbl.add_exn t.addr2loc ~key:addr ~data:loc

let create locations (agg : Aggregated_perf_profile.t) =
  if !verbose then printf "Decoding perf profile.\n";
  (* Collect all addresses that need decoding. Mispredicts and traces use the
     same addresses as branches, so no need to add them *)
  (* Overapproximation of number of different addresses for creating hashtbl *)
  let size =
    Hashtbl.length agg.instructions + (Hashtbl.length agg.branches * 2)
  in
  let addresses = Hashtbl.create ~size (module Addr) in
  let add key =
    if not (Hashtbl.mem addresses key) then (
      if !verbose then printf "Adding key 0x%Lx\n" key;
      Hashtbl.set addresses ~key ~data:None )
    else if !verbose then printf "Found key 0x%Lx\n" key
  in
  let add2 (fa, ta) =
    add fa;
    add ta
  in
  Hashtbl.iter_keys agg.instructions ~f:add;
  Hashtbl.iter_keys agg.branches ~f:add2;

  (* A key may be used multiple times in keys of t.instruction and t.branches *)
  let len = Hashtbl.length addresses in
  if !verbose then printf "size=%d,len=%d\n" size len;
  assert (len <= size);
  (* Resolve all addresses seen in samples in one pass over the binary. *)
  Elf_locations.resolve_all locations addresses;
  let crcs = Crcs.(mk Create) in
  Elf_locations.iter_symbols locations ~f:(Crcs.decode_and_add_symbol crcs);
  let t = mk len (Crcs.tbl crcs) agg.buildid in
  (* Decode all locations: map addresses to locations. *)
  Hashtbl.iteri addresses ~f:(fun ~key:addr ~data:dbg ->
      (* this is cached using interval tree *)
      let interval =
        Elf_locations.resolve_function_containing locations
          ~program_counter:addr
      in
      decode_addr t addr interval dbg);
  create_func_execounts t agg;
  t

let read filename =
  if !verbose then
    printf "Reading aggregated decoded profile from %s\n" filename;
  let t =
    match Parsexp_io.load (module Parsexp.Single) ~filename with
    | Ok t_sexp -> t_of_sexp t_sexp
    | Error error ->
        Parsexp.Parse_error.report Caml.Format.std_formatter error ~filename;
        Report.user_error "Cannot parse aggregated decoded profile file"
  in
  if !verbose then printf !"Aggregated decoded profile:\n%{sexp:t}\n" t;
  t

let write t filename =
  if !verbose then
    printf "Writing aggregated decoded profile to %s\n" filename;
  let chan = Out_channel.create filename in
  Printf.fprintf chan !"%{sexp:t}\n" t;
  Out_channel.close chan

let top_functions t =
  (* Sort functions using preliminary function-level execution counts in
     descending order. *)
  let sorted = List.sort (Hashtbl.data t.functions) ~compare:Func.compare in
  let fl = List.map sorted ~f:(fun func -> func.name) in
  fl

(* Translate linear ids of this function's locations to cfg labels within
   this function, find the corresponding basic blocks and update their
   block_info. Perform lots of sanity checks to make sure the location of the
   execounts match the instructions in the cfg. *)
let create_cfg_info t func cl =
  let get_loc addr = Hashtbl.find_exn t.addr2loc addr in
  let i = Cfg_info.create cl func in
  (* Associate instruction counts with basic blocks *)
  Hashtbl.iteri func.agg.instructions ~f:(fun ~key ~data ->
      let loc = get_loc key in
      Cfg_info.record_ip i ~loc ~data);

  (* Associate fall-through trace counts with basic blocks *)
  Hashtbl.iteri func.agg.traces ~f:(fun ~key ~data ->
      let from_addr, to_addr = key in
      let from_loc = get_loc from_addr in
      let to_loc = get_loc to_addr in
      Cfg_info.record_trace i ~from_loc ~to_loc ~data);
  ( if !verbose then
    let total_traces =
      List.fold (Hashtbl.data func.agg.traces) ~init:0L ~f:Int64.( + )
    in
    let ratio =
      if Int64.(total_traces > 0L) then
        Int64.(func.malformed_traces * 100L / total_traces)
      else 0L
    in
    printf "Found %Ld malformed traces out of %Ld (%Ld%%)\n"
      func.malformed_traces total_traces ratio );

  (* Associate branch counts with basic blocks *)
  Hashtbl.iteri func.agg.branches ~f:(fun ~key ~data ->
      let mispredicts =
        Option.value (Hashtbl.find func.agg.mispredicts key) ~default:0L
      in
      let from_addr, to_addr = key in
      let from_loc = get_loc from_addr in
      let to_loc = get_loc to_addr in
      Cfg_info.record_branch i ~from_loc ~to_loc ~data ~mispredicts);
  Cfg_info.blocks i

(* Compute detailed execution counts for function [name] using its CFG *)
let add t name cl =
  match Hashtbl.find t.name2id name with
  | None ->
      if !verbose then printf "Not found profile for %s with cfg.\n" name;
      None
  | Some id ->
      let func = Hashtbl.find_exn t.functions id in
      if Int64.(func.count > 0L) && func.has_linearids then (
        if !verbose then (
          printf "compute_cfg_execounts for %s\n" name;
          Cfg_with_layout.print_dot cl "execount" );
        let cfg_info = create_cfg_info t func cl in
        Hashtbl.add_exn t.execounts ~key:id ~data:cfg_info;
        Some cfg_info )
      else None

let rename t old2new =
  if not (Hashtbl.is_empty t.execounts) then
    Report.user_error "Rename of execounts is not implemented";
  Hashtbl.map_inplace t.name2id ~f:(fun id -> Hashtbl.find_exn old2new id);
  Hashtbl.map_inplace t.addr2loc ~f:(Loc.rename ~old2new);
  let size = Hashtbl.length t.functions in
  let functions = Hashtbl.create ~size (module Int) in
  Hashtbl.iteri old2new ~f:(fun ~key:id ~data:newid ->
      let func = Hashtbl.find_exn t.functions id in
      Hashtbl.set functions ~key:newid ~data:{ func with id = newid });
  assert (Hashtbl.length functions = size);
  { t with functions }

(* CR-soon gyorsh: use information from t.crcs when merging functions.
   WARNING: modifies the input profiles inplace, to avoid allocations.

   If crc checks are disabled (-no-md5 command line option), then a mismatch
   in crcs is reported in verbose and then we can do one of the following
   alternatives:

   1) ignore the mismatch: functions are considered identical for the purpose
   of merging their execution counts. 2) keep both copies of the function,
   and use the one that applies when compiling with fdo. 3) remove both
   functions from the profile when there is a mismatch because we do not know
   which is the current/more recent. 4) keep only the most recent, as
   specified by the user (need extra argument).

   Current implementation is effectively (1), because CRC are not used when
   merging the functions. CR-soon gyorsh: enable the above alternatives
   through a command line option, for experiments.

   When ignoring buildid and crcs, it is possible to merge profiles from
   binaries that differ only in addresses, for example, profiles collected
   from different layouts of the same code. *)
let merge_into ~src ~dst ~unit_crc ~func_crc ~ignore_buildid =
  dst.buildid <- Merge.buildid src.buildid dst.buildid ~ignore_buildid;
  (* refresh ids of function in src, so as not to clash with dst: if func is
     in both src and dst, use the id from dst, otherwise rename src id to a
     fresh id. *)
  let fresh = ref (Hashtbl.length dst.name2id) in
  let old2new =
    Hashtbl.create ~size:(Hashtbl.length src.name2id) (module Int)
  in
  let merge_name2id ~key:_ a b =
    let newid =
      match b with
      | None ->
          let newid = !fresh in
          fresh := !fresh + 1;
          newid
      | Some b -> b
    in
    Hashtbl.add_exn old2new ~key:a ~data:newid;
    Hashtbl.Set_to newid
  in
  if !verbose then (
    Printf.printf !"src:\n%{sexp:(string, int) Hashtbl.t}\n" src.name2id;
    Printf.printf !"dst:\n%{sexp:(string, int) Hashtbl.t}\n" dst.name2id );
  Hashtbl.merge_into ~src:src.name2id ~dst:dst.name2id ~f:merge_name2id;
  if !verbose then
    Printf.printf !"old2new:\n%{sexp:(int, int) Hashtbl.t}\n" old2new;
  let src = rename src old2new in
  if !verbose then (
    Printf.printf !"src:\n%{sexp:(string, int)Hashtbl.t}\n" src.name2id;
    Printf.printf !"dst:\n%{sexp:(string, int) Hashtbl.t}\n" dst.name2id );
  let merge_addr2loc ~key:_ a = function
    | None -> Hashtbl.Set_to a
    | Some b -> Hashtbl.Set_to (Loc.merge a b)
  in
  let merge_functions ~key:_ a = function
    | None -> Hashtbl.Set_to a
    | Some b ->
        Hashtbl.Set_to (Func.merge a b ~unit_crc ~func_crc ~ignore_buildid)
  in
  let merge_execounts ~key:_ _a =
    Report.user_error "Merge of cfg_info is not supported yet"
  in
  Hashtbl.merge_into ~src:src.addr2loc ~dst:dst.addr2loc ~f:merge_addr2loc;
  Hashtbl.merge_into ~src:src.functions ~dst:dst.functions ~f:merge_functions;
  Hashtbl.merge_into ~src:src.execounts ~dst:dst.execounts ~f:merge_execounts;
  Crcs.merge_into ~src:src.crcs ~dst:dst.crcs ~unit_crc ~func_crc

module Merge = Merge.Make (struct
  type nonrec t = t

  let merge_into = merge_into

  let read = read

  let write = write

  let approx_size t = Hashtbl.length t.addr2loc + Hashtbl.length t.name2id
end)
