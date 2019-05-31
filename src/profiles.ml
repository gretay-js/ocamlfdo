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

module Aggregated = struct
  type loc = {
    func : string;  (* Name of the containing function *)
    linearid : int;
  } [@@deriving compare, sexp]
  type branch = {
    from_loc : loc;
    to_loc : loc;
    mispredict : int; (* number of times the branch was mispredicted:
                         branch target mispredicted or
                         branch direction was mispredicted. *)
    execount : int ;  (* execution counter: branch was taken. *)
  } [@@deriving compare, sexp]

  let create _decoded_profile =
    failwith "Not implemented"
end

module Decoded = struct
  type loc = {
    addr : int64;   (* Raw address in the original binary *)
    func : string;  (* Name of the containing function *)
    offset : int;   (* Offset from the start of the function *)
    (* Dwarf info associated with a location *)
    file : string;  (* filename *)
    line : int;     (* line number *)
  } [@@deriving compare, sexp]
  (* Branch *)
  type br = {
    from_loc : loc option;
    to_loc : loc option;
    mispredicted : bool;
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
    Out_channel.close chan;

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
    (* cycles : int; *)
  } [@@deriving compare, sexp]

  type sample = {
    ip: int64;  (* instruction pointer where the sample was taken *)
    brstack: br list; (* branch stack is the last branch record (LBR) *)
  } [@@deriving compare, sexp]

  type t = sample list
  [@@deriving compare, sexp]

  let parse_br s =
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
        mispredict;
      }
    | _ -> failwithf "Cannot parse %s\n" s ()

  let print t =
    Printf.printf !"%{sexp:t}\n" t

  (* This function reverse the stack from its perf profile order *)
  let rec parse_brstack brstack row =
    match row with
    | [] -> brstack
    | hd::tl ->
      let brstack = (parse_br hd)::brstack
      in parse_brstack brstack tl

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
          brstack = parse_brstack [] rest;
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

  let decode_loc locations addr =
    match Elf_locations.resolve_function_containing locations
            ~program_counter:addr with
    | None -> None
    | Some interval ->
      let func = interval.v in
      let open Ocaml_locations in
      match decode_line locations ~program_counter:addr func Linearid with
      | None -> None
      | Some (file,line) ->
        let start = interval.l in
        match Int64.(to_int (addr - start)) with
        | None -> failwithf "Offset to big: 0x%Lx" (Int64.(addr - start)) ()
        | Some offset ->
          assert (offset >= 0);
          let open Decoded in
          Some {
            addr;
            func;
            offset;
            file;
            line;
          }

  let decode_brstack locations brstack =
    List.map brstack
      ~f:(fun br ->
        let from_loc = decode_loc locations br.from_addr in
        let to_loc = decode_loc locations br.to_addr in
        let mispredicted = mispredicted br.mispredict in
        let open Decoded in
        { from_loc; to_loc; mispredicted; }
      )

  let decode t locations =
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
    List.filter_map t
      ~f:(fun sample ->
          match decode_loc locations sample.ip with
          | None -> None
          | Some ip ->
            let brstack = decode_brstack locations sample.brstack in
            let open Decoded in Some { ip; brstack; }
        )
end
