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
open Func

let verbose = ref true

type t = {
  (* manipulate raw binary symbols and addresses with their debug info *)
  locations : Elf_locations.t;
  (* map raw addresses to locations *)
  addr2loc : Loc.t Hashtbl.M(Addr).t;
  (* map func name to func id *)
  name2id : int Hashtbl.M(String).t;
  (* map func id to func info *)
  functions : Func.t Hashtbl.M(Int).t;
  (* number of fallthrough traces between functions *)
  mutable malformed_traces : Execount.t
}
[@@deriving sexp]

let mk size locations =
  { addr2loc = Hashtbl.create ~size (module Addr);
    name2id = Hashtbl.create (module String);
    functions = Hashtbl.create (module Int);
    malformed_traces = 0L;
    locations
  }

let get_func t addr =
  let loc = Hashtbl.find_exn t.addr2loc addr in
  match loc.rel with
  | None -> None
  | Some rel ->
      let id = rel.id in
      let func = Hashtbl.find_exn t.functions id in
      Some func

(* Partition aggregated_perf to functions and calculate total execution
   counts of each function. Total execution count of a function is
   determined from the execution counts of samples contained in this
   function. It uses LBR info: if a branch source or target is contained in
   the function, it contributes to execution count of the function. It does
   not use the CFG. In particular, it does not count instructions that can
   be traced using LBR. The advantage is that we can compute it for
   non-OCaml functions. *)
let create_func_execounts t (agg : Aggregated_perf.t) =
  Hashtbl.iteri agg.instructions ~f:(fun ~key ~data ->
      match get_func t key with
      | None -> ()
      | Some func ->
          func.count <- Int64.(func.count + data);
          Hashtbl.add_exn func.agg.instructions ~key ~data );
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
        Hashtbl.add_exn func.agg.mispredicts ~key ~data:mispredicts
      in
      process key update_br );
  Hashtbl.iteri agg.traces ~f:(fun ~key ~data ->
      (* traces don't contribute to func's total count because it is account
         for in branches. *)
      let update_tr func = Hashtbl.add_exn func.agg.traces ~key ~data in
      process key update_tr )

(* Find or add the function and return its id *)
let get_func_id t ~name ~start =
  match Hashtbl.find t.name2id name with
  | None ->
      let id = Hashtbl.length t.functions in
      let func = Func.mk ~id ~name ~start in
      Hashtbl.add_exn t.functions ~key:id ~data:func;
      Hashtbl.add_exn t.name2id ~key:name ~data:id;
      func.id
  | Some id ->
      let func = Hashtbl.find_exn t.functions id in
      assert (func.id = id);
      assert (func.name = name);
      assert (func.start = start);
      func.id

let decode_loc t addr =
  let open Loc in
  match
    Elf_locations.resolve_function_containing t.locations
      ~program_counter:addr
  with
  | None ->
      if !verbose then
        printf "Cannot find function symbol containing 0x%Lx\n" addr;
      { addr; rel = None; dbg = None }
  | Some interval ->
      let name = interval.v in
      let start = interval.l in
      let offset =
        match Int64.(to_int (addr - start)) with
        | None -> failwithf "Offset too big: 0x%Lx" Int64.(addr - start) ()
        | Some offset ->
            assert (offset >= 0);
            offset
      in
      let id = get_func_id t ~name ~start in
      let rel = Some { id; offset; label = None } in
      let dbg =
        match
          Ocaml_locations.(
            decode_line t.locations ~program_counter:addr name Linearid)
        with
        | None -> None
        | Some (file, line) ->
            (* Set has_linearids of this function *)
            let func = Hashtbl.find_exn t.functions id in
            func.has_linearids <- true;
            Some { Loc.file; line }
      in
      { addr; rel; dbg }

let create l (aggregated_perf : Aggregated_perf.t) =
  if !verbose then printf "Decoding perf profile.\n";
  (* Collect all addresses that need decoding. Mispredicts and traces use
     the same addresses as branches, so no need to add them *)
  (* Overapproximation of number of different addresses for creating hashtbl *)
  let len =
    Hashtbl.length aggregated_perf.instructions
    + (Hashtbl.length aggregated_perf.branches * 2)
  in
  (* Elf_locations does not use Core, so we need to create Caml.Hashtbl *)
  let addresses = Caml.Hashtbl.create len in
  let add key =
    if not (Caml.Hashtbl.mem addresses key) then
      Caml.Hashtbl.add addresses key ()
    else printf "Found key 0x%Lx\n" key
  in
  let add2 (fa, ta) =
    add fa;
    add ta
  in
  Hashtbl.iter_keys aggregated_perf.instructions ~f:add;
  Hashtbl.iter_keys aggregated_perf.branches ~f:add2;
  (* A key may be used multiple times in keys of t.instruction and
     t.branches *)
  let size = Caml.Hashtbl.length addresses in
  if !verbose then printf "size=%d,len=%d\n" size len;
  assert (size <= len);
  let t = mk size l in
  (* Resolve and cache all addresses we need in one pass over the binary. *)
  Elf_locations.resolve_all t.locations addresses;
  (* Decode all locations: map addresses to locations *)
  Caml.Hashtbl.iter
    (fun addr _ ->
      let loc = decode_loc t addr in
      Hashtbl.add_exn t.addr2loc ~key:addr ~data:loc )
    addresses;
  create_func_execounts t aggregated_perf;
  (* To free space, reset the cache we used for decoding dwarf info. We may
     use locations later for printing to bolt format for testing/debugging.*)
  Elf_locations.reset t.locations;
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
  if !verbose then
    Printf.printf !"Aggregated decoded profile:\n%{sexp:t}\n" t;
  t

let write t filename =
  if !verbose then
    printf "Writing aggregated decoded profile to %s\n" filename;
  let chan = Out_channel.create filename in
  Printf.fprintf chan !"%{sexp:t}\n" t;
  Out_channel.close chan

let write_top_functions t filename =
  if !verbose then printf "Writing top functions to %s\n" filename;
  (* Sort functions using preliminary function-level execution counts in
     descending order. *)
  let sorted = List.sort (Hashtbl.data t.functions) ~compare:Func.compare in
  let fl = List.map sorted ~f:(fun func -> func.name) in
  Layouts.Func_layout.write_linker_script fl filename
