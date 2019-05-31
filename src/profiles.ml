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

(* Dwarf info associated with a location *)
type dbg = {
    file : string;  (* filename *)
    line : int;     (* line number *)
  } [@@deriving compare, sexp]

type loc = {
    addr : int64;   (* Raw address in the original binary *)
    func : string;  (* Name of the containing function symbol *)
    offset : int;   (* Offset from the start of the function *)
    dbg : dbg option;
  } [@@deriving compare, sexp]

(* CR gyorsh: aggregated_perf and aggregated_decoded have the same structure.
   Generalize the type? *)
module 'k Aggregated = struct

  type t = {
    instructions : (k, int64) Hashtbl.t;
    branches : (k * k, int64) Hashtbl.t;
    (* execution count: number of times the branch was taken. *)
    mispredicts : (k * k, int64) Hashtbl.t;
    (* number of times the branch was mispredicted:
       branch target mispredicted or
       branch direction was mispredicted. *)
    traces : (k * k, int64) Hashtbl.t;
    (* execution count: number of times the trace was taken. *)
  }

  module S = struct
    type t = k [@@deriving compare, hash, sexp]
  end
  module P = struct
    type t = k*k [@@deriving compare, hash, sexp]
  end

  let empty =
    { instructions = Hashtbl.create (module S);
      branches = Hashtbl.create (module P);
      mispredicts = Hashtbl.create (module P);
      traces = Hashtbl.create (module P);
    }
end

module Aggregated_perf = struct

  type t = {
    instructions : (int64, int64) Hashtbl.t;
    branches : (int64 * int64, int64) Hashtbl.t;
    (* execution count: number of times the branch was taken. *)
    mispredicts : (int64 * int64, int64) Hashtbl.t;
    (* number of times the branch was mispredicted:
       branch target mispredicted or
       branch direction was mispredicted. *)
    traces : (int64 * int64, int64) Hashtbl.t;
    (* execution count: number of times the trace was taken. *)
  }

  module P = struct
    type t = int64*int64 [@@deriving compare, hash, sexp]
  end

  let empty =
    { instructions = Hashtbl.create (module Int64);
      branches = Hashtbl.create (module P);
      mispredicts = Hashtbl.create (module P);
      traces = Hashtbl.create (module P);
    }
end

module Aggregated_decoded = struct

  type instruction = {
    loc : loc;
    execount : int;
  } [@@deriving compare, sexp]

  type branch = {
    from_loc : loc;
    to_loc : loc;
    mispredicts : int; (* number of times the branch was mispredicted:
                         branch target mispredicted or
                         branch direction was mispredicted. *)
    execount : int ;  (* execution count: number of times the branch was taken. *)
  } [@@deriving compare, sexp]

  type trace = {
    from_loc : loc;
    to_loc : loc;
    execount : int;
  } [@@deriving compare, sexp]

  type t = {
    instructions : instruction list;
    branches : branch list;
    traces : trace list;
  } [@@deriving compare, sexp]

  let empty =
    { instructions=[];
      branches=[];
      traces=[];
    }

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

module Decoded_perf = struct
  (* Branch *)
  type br = {
    from_loc : loc;
    to_loc : loc;
    mispredicted : bool;
    index : int;
  } [@@deriving compare, sexp]

  type sample = {
    ip: loc; (* instruction pointer where the sample was taken *)
    brstack: br list;
  } [@@deriving compare, sexp]

  type t = sample list
  [@@deriving compare, sexp]

  let read filename =
    if !verbose then
      printf "Reading decoded profile from %s\n" filename;
    let t =
      match Parsexp_io.load (module Parsexp.Single) ~filename with
      | Ok t_sexp -> t_of_sexp t_sexp
      | Error error ->
        Parsexp.Parse_error.report Caml.Format.std_formatter error ~filename;
        failwith "Cannot parse decoded profile file"
    in
    if !verbose then begin
      Printf.printf !"Decoded profile:\n%{sexp:t}\n" t;
      if t = [] then Printf.printf "Empty layout!\n"
    end;
    t

  let write t filename =
    let chan = Out_channel.create filename in
    Printf.fprintf chan !"%{sexp:t}\n" t;
    Out_channel.close chan
end

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

  let row_to_sample ~keep_pid t row =
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
        sample::t
      end
      else
        t
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
    let t = In_channel.fold_lines chan ~init:[] ~f:(row_to_sample ~keep_pid) in
    In_channel.close chan;
    if !verbose then begin
      print t;
      Printf.printf "Found pids:\n";
      Int.Set.iter !pids ~f:(fun pid -> printf "%d\n" pid)
    end;
    t

end

let decode_loc locations addr =
  let interval =
    match Elf_locations.resolve_function_containing locations
            ~program_counter:addr with
    | None ->
      failwithf "Cannot find function symbol containing 0x%Lx" addr ()
    | Some interval -> interval in
  let func = interval.v in
  let dbg =
    match Ocaml_locations.(decode_line locations
                             ~program_counter:addr func Linearid) with
    | None -> None
    | Some (file,line) -> Some {file;line;} in
  let start = interval.l in
  let offset =
    match Int64.(to_int (addr - start)) with
      | None -> failwithf "Offset too big: 0x%Lx" (Int64.(addr - start)) ()
      | Some offset -> assert (offset >= 0); offset in
  {
    addr;
    func;
    offset;
    dbg;
  }

let decode_brstack locations (brstack:Perf.br list) =
  List.map brstack
    ~f:(fun br ->
      let from_loc = decode_loc locations br.from_addr in
      let to_loc = decode_loc locations br.to_addr in
      let mispredicted = Perf.mispredicted br.mispredict in
      let open Decoded_perf in
      { from_loc; to_loc; mispredicted; index=br.index; }
    )

let decode_perf locations (t:Perf.t) =
  let open Perf in
  if !verbose then
    printf "Decoding perf profile.\n";

  (* Collect all addresses that need decoding. *)
  let len = List.length t in
  let addresses = Caml.Hashtbl.create len in
  List.iter t
    ~f:(fun sample ->
      Caml.Hashtbl.add addresses sample.ip ();
      List.iter sample.brstack
        ~f:(fun br ->
          Caml.Hashtbl.add addresses br.from_addr ();
          Caml.Hashtbl.add addresses br.to_addr ());
    );
  (* Resolve and cache all addresses we need in one pass over the binary. *)
  Elf_locations.resolve_all locations addresses ~reset:true;
  (* Decode all samples *)
  List.map t
    ~f:(fun sample ->
      let ip = decode_loc locations sample.ip in
      let brstack = decode_brstack locations sample.brstack in
      let open Decoded_perf in { ip; brstack; }
    )

let decode_aggregated locations (t:Aggregated_perf.t) =
  if !verbose then
    printf "Decoding perf profile.\n";

  (* Collect all addresses that need decoding. *)
  let len = Hashtbl.length t.instruction +
            Hastbl.length t.branches in
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
  (* Copy aggregated profile, mapping addresses to locations in the process *)
  let decoded = Aggregated_decoded.empty in
  let map ~table ~key ~data =
    let loc = Hashtbl.findo_or_add addr2loc key ~default:(decode_loc locations) in
    Hashtbl.add_exn table ~key:loc ~data
  in
  let map2 ~table ~key ~data =
    let (l,r) = key in
    let locl = Hashtbl.findi_or_add addr2loc l ~default:(decode_loc locations) in
    let locr = Hashtbl.findi_or_add addr2loc r ~default:(decode_loc locations) in
    Hashtbl.add_exn table ~key:(locl,locr) ~data
  in
  Hashtbl.iteri t.instructions ~f:(map ~table:decoded.instructions);
  Hashtbl.iteri t.branches ~f:(map2 ~table:decoded.branches);
  Hashtbl.iteri t.mispredicts ~f:(map2 ~table:decoded.mispredicts);
  Hashtbl.iteri t.traces ~f:(map2 ~table:decoded.traces);
  decoded

let aggregate_perf (t:Perf.t) =
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

let aggregate_decoded (_t:Decoded_perf.t) =
  failwith "Not implemented aggregate of decoded perf profile"
