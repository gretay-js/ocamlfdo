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

let verbose = ref true

module Kind = struct
  type t =
    | UnknownSymbol
    | Symbol
    | UnknownMem
    | Mem
  [@@deriving compare, sexp]

  let to_string = function
    | UnknownSymbol -> "0"
    | Symbol -> "1"
    | UnknownMem -> "3"
    | Mem -> "4"

  let of_string = function
    | "0" -> UnknownSymbol
    | "1" -> Symbol
    | "3" -> UnknownMem
    | "4" -> Mem
end

module Bolt_loc = struct
  type t = {
    kind : Kind.t;
    name : string;
    offset : int
  }
  [@@deriving fields, csv, compare, sexp]

  let to_string t =
    sprintf "%s %s %s" (to_string kind) name (to_string offset)
end

module Bolt_branch = struct
  type t = {
    src : bolt_loc;
    dst : bolt_loc;
    mis : Int64.t;
    count : Int64.t
  }
  [@@deriving fields, csv, compare, sexp]
end

type t = Bolt_branch.t list

let print t = Printf.printf !"%{sexp:t}\n" t

let read filename =
  if !verbose then printf "Reading bolt fdata from %s\n" filename;
  let t = csv_load ~separator:' ' filename in
  if !verbose then print t;
  p

(* Try to get the raw address in the original binary and create Loc.t with
   it. It's used for debugging to generating bolt fdata. *)
let find_loc t func line =
  (* Reverse lookup in t.addr2loc is probably more expensive, because we
     have already cached inverse. *)
  let addr = Ocaml_locations.to_address locations func.name line in
  Hashtbl.find_or_add t.addr2loc ~key:addr ~default:(fun () ->
      let rel = Some { id = func.id; offset = Int64.(addr - func.start) } in
      let dbg = Some { file = func.name; line } in
      let loc = { addr; rel; dbg } in
      Hasthbl.add_exn t.addr2loc ~key:addr ~data:loc )

(* For each function, check its execounts and collect inferred fallthrough
   edges. *)
let fallthroughs locations (p : Aggregated_decoded.t) =
  Hashtbl.fold t.functions ~init:[] ~f:(fun acc func ->
      if not (List.is_empty func.fallthroughs) then (
        (* This is expensive because we need to scan the entire original
           binary to find the function. *)
        match
          Elf_locations.resolve inverse locations func.start func.finish
        with
        | None ->
            if !verbose then
              printf "Cannot find function symbol containing 0x%Lx\n" addr;
            { addr; rel = None; dbg = None }
        | Some interval ->
            let name = interval.v in
            let start = interval.l in
            Elf_locations.resolve_function_starting_at locations
              ~program_counter:func.start ~reset:true ~contents:true
              ~create_inverse:true;
            List.fold (Func.fallthroughs locations func) ~init:map
              ~f:(fun acc ft ->
                let src_addr = inverse func ft.src in
                let dst_addr = inverse func ft.dst in
                (src_addr, dst_addr, ft.count) :: acc ) )
      else acc )

let mk (p : Aggregated_decoded.t) agg =
  let get_loc addr = Hashtbl.find_exn p.addr2loc addr in
  let get_bolt_loc loc =
    match loc.rel with
    | None -> { kind = UnknownSymbol; name = "[unknown]"; offset = 0 }
    | Some rel ->
        let func = Hashtbl.find_exn p.functions rel.id in
        { kind = Symbol; name = func.name; offset = rel.offset }
  in
  let append_if_valid acc src_loc dst_loc count mis =
    let src = get_bolt_loc src_loc in
    let dst = get_bolt_loc dst_loc in
    if src.kind = UnknownSymbol && dst.kind = UnkownSymbol then acc
    else
      let b = { src; dst; mis; count } in
      b :: acc
  in
  let t =
    Hashtbl.fold agg.branches ~init:[]
      ~f:(fun acc ~key:(src, dst) ~data:count ->
        let mis =
          Option.value (Hashtbl.find agg.mispredicts key) ~default:0L
        in
        let src_loc = get_loc src in
        let dst_loc = get_loc dst in
        append_if_valid acc src_loc dst_loc count mis )
  in
  List.fold (fallthroughs p) ~init:t ~f:(fun (src_loc, dst_loc, count) ->
      append_if_valid acc src_loc dst_loc count 0L )

let write p agg filename =
  let open Loc in
  if !verbose then
    printf
      "Writing preliminary aggregated decoded profile in bolt form to %s\n"
      filename;
  let chan = Out_channel.create filename in
  let t = mk p agg in
  (* If [t] uses too much memory and we don't need it other than for
     printing, then we can print during [mk] instead of "append_if" above *)
  List.iter t ~f:(fun branch ->
      fprintf chan "%s\n" (Bolt_branch.to_string bolt_branch) );
  Out_channel.close chan

(* Parse the file direclty without csv *)
(* let read_kind s =
 *   match Int.of_string src_kind *)

(* let row_to_sample row =
 *   match String.split ~on:' ' row with
 *   | src_kind::src_name::src_offset::dst_kind::dst_name::dst_offset::mis::count ->
 *     let src_kind =
 *     let src =    {
 *        = int
 *      } *)
(* let read filename =
 *   if !verbose then
 *     printf
 *       "Reading bolt fdata profile from %s\n"
 *       filename;
 *   let chan = In_channel.create filename in
 *   let t =
 *     In_channel.fold_lines chan ~init:[]
 *       ~f:(fun acc r -> (row_to_sample r)::acc)
 *   in
 *   In_channel.close chan;
 *   if !verbose then
 *     print t;
 *   t *)
