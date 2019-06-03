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

  type func = {
    id : int;       (* Unique identifier we assign to this function *)
    name : string;  (* Name of thefunction symbol *)
    start : int;    (* Raw start address of the function inthe original binary *)
  } [@@deriving compare, sexp, hash]

  let get_func_id functions ~name ~start =
    match Hashtbl.find functions name with
    | None ->
      let id = Hashtbl.length functions in
      Hashtbl.set functions ~key:name ~data:{id;name;start}
    | Some func ->
      assert (func.start = start);
      func.id

  type rel = {
    id : int;       (* Unique id of the containing function symbol *)
    offset : int;   (* Offset from the start of the function *)
  } [@@deriving compare, sexp, hash]

  type t = {
    addr : int64;   (* Raw address in the original binary *)
    rel : rel option;    (* Containing function info and relative offset *)
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
    type t = {
      instructions : int64 Hashtbl.M(Int64).t;
      branches : int64 Hashtbl.M(P).t;
      (* execution count: number of times the branch was taken. *)
      mispredicts : int64 Hashtbl.M(P).t;
      (* number of times the branch was mispredicted:
         branch target mispredicted or
         branch direction was mispredicted. *)
      traces : int64 Hashtbl.M(P).t;
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

module Aggregated_decoded = struct
  type t =
  { counts : Aggregated_perf.t;
    addr2loc : Loc.t Hashtbl.M(Int64).t;
    functions : Loc.func Hashtbl.M(String).t;
  }  [@@deriving sexp]

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

  let loc t addr = Hashtbl.find_exn t.addr2loc addr
end

type t = Aggregated_decoded.t

let write_top_functions t filename =
  if !verbose then
    printf "Writing top functions to %s\n" filename;
  let chan = Out_channel.create filename in
  let locs = Hashtbl.data t.addr2loc in
  List.fold t.addr2loc ~init:String.Map.empty
    ~f:(fun loc ...
  Out_channel.close chan

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

let decode_loc functions locations addr =
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
    let id = Loc.get_func_id functions name start in
    let func = Some { id; offset; } in
    let dbg =
      match Ocaml_locations.(decode_line locations
                               ~program_counter:addr name Linearid) with
      | None -> None
      | Some (file,line) -> Some {Loc.file;line;} in
    { addr; func; dbg; }

let decode_aggregated locations (t:Aggregated_perf.t) =
  if !verbose then
    printf "Decoding perf profile.\n";

  (* Collect all addresses that need decoding. *)
  let len = Hashtbl.length t.instructions +
            Hashtbl.length t.branches in
  let addresses = Caml.Hashtbl.create len in
  let add key =
    if not (Caml.Hashtbl.mem addresses key) then
      Caml.Hashtbl.add addresses key ();
  in
  let add2 (f,t) = add f; add t in
  Hashtbl.iter_keys t.instructions ~f:add;
  Hashtbl.iter_keys t.branches ~f:add2;
  (* Mispredicts and traces use the same addresses as branches, no need to add them *)
  let size = Caml.Hashtbl.length addresses in
  (* Resolve and cache all addresses we need in one pass over the binary. *)
  Elf_locations.resolve_all locations addresses ~reset:true;
  (* Decode all locations *)
  let addr2loc = Hashtbl.create ~size (module Int64) in
  (* Mapping addresses to locations *)
  Caml.Hashtbl.iter (fun key _ ->
    let data = decode_loc functions locations key in
    Hashtbl.set addr2loc ~key ~data)
    addresses;
  {Aggregated_decoded.counts=t;addr2loc;functions;}

let _aggregate_perf (t:Perf.t) =
  if !verbose then
    printf "Aggregate perf profile.\n";
  let inc table key =
    Hashtbl.update table key
      ~f:(fun v -> Int64.(1L + Option.value ~default:0L v)) in
  let aggregated = Aggregated_perf.empty in
  (* CR gyorsh: aggregate during parsing of perf profile *)
  let aggregate (sample:Perf.sample) =
    inc aggregated.instructions sample.ip;
    List.iter sample.brstack
      ~f:(fun br ->
        let key = (br.from_addr,br.to_addr) in
        inc aggregated.branches key;
        if Perf.mispredicted br.mispredict then
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
         ): Perf.br option)
  in
  List.iter t ~f:aggregate;
  aggregated
