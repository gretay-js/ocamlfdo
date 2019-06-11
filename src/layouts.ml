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

let verbose = ref false

module Raw_layout : sig
  (* CR gyorsh: it has to be named t for csv fields ppx to work. move to
     separate module. *)
  type t = {
    address : int64;
    (* start of function *)
    offset : int;
    (* offset from address *)
    index : int;
    (* index in the original layout *)
    position : int (* new position in the permutation *)
  }
  [@@deriving fields, csv, compare, sexp]

  (* We don't need all the fields, but redundancy is used for validating the
     input. *)
  type p = t list [@@deriving sexp]

  val read : string -> p

  val print : p -> unit

  val print_t : t -> unit
end = struct
  type t = {
    address : int64;
    (* start of function *)
    offset : int;
    (* offset from address *)
    index : int;
    (* index in the original layout *)
    position : int (* new position in the permutation *)
  }
  [@@deriving fields, csv, compare, sexp]

  type p = t list [@@deriving sexp]

  let print_t i =
    Printf.printf "0x%Lx:0x%x:%d:%d\n" i.address i.offset i.position
      i.position

  let print l =
    (* List.iter l ~f:print_t; *)
    Printf.printf !"%{sexp:p}\n" l

  let read filename =
    if !verbose then printf "Reading raw layout from %s\n" filename;
    let p = csv_load ~separator:':' filename in
    if !verbose then print p;
    p
end

module Rel_layout : sig
  type t = {
    func : string;
    labels : int list
  }
  [@@deriving sexp]

  type p = t list [@@deriving sexp]

  val read : string -> p

  val writer : string option -> string -> int list -> unit
end = struct
  type t = {
    func : string;
    labels : int list
  }
  [@@deriving sexp]

  type p = t list [@@deriving sexp]

  let print_t t outc = fprintf outc !"%{sexp:t}\n" t

  let print_p p outc = List.iter p ~f:(fun t -> print_t t outc)

  let read filename =
    if !verbose then printf "Reading layout from %s\n" filename;
    let p =
      match Parsexp_io.load (module Parsexp.Many) ~filename with
      | Ok p_sexp_list -> List.map p_sexp_list ~f:t_of_sexp
      | Error error ->
          Parsexp.Parse_error.report Caml.Format.std_formatter error
            ~filename;
          failwith "Cannot parse relative layout file"
    in
    (* let p = csv_load ~separator:':' filename in *)
    if !verbose then (
      print_p p Out_channel.stdout;
      if p = [] then Printf.printf "Empty layout!\n" );
    p

  let writer filename =
    match filename with
    | None -> fun _ _ -> ()
    | Some filename ->
        (* CR gyorsh: hack to erase pervious contents: open and immediate
           close the file. *)
        Out_channel.close (Out_channel.create filename);
        fun func labels ->
          let chan = Out_channel.create filename ~append:true in
          print_t { func; labels } chan;
          Out_channel.close chan
end

let convert_layout (l : Rel_layout.p) =
  List.fold l ~init:String.Map.empty ~f:(fun layout p ->
      String.Map.add_exn layout ~key:p.func ~data:p.labels )

let print_fun_layout_item (key, data) =
  Printf.printf "position=%d linear_id=%d\n" key data

let print_fun_layout ~key:name ~data:(fun_layout : (int, int) Hashtbl.t) =
  Printf.printf "%s (%d)\n" name (Hashtbl.length fun_layout);
  let sorted_fun_layout =
    List.sort (Hashtbl.to_alist fun_layout) ~compare:(fun (k1, _) (k2, _) ->
        Int.compare k1 k2 )
  in
  List.iter sorted_fun_layout ~f:print_fun_layout_item

let print_layout layout = Hashtbl.iteri layout ~f:print_fun_layout

let _to_func file =
  match String.chop_suffix file ~suffix:".linear" with
  | None -> None
  | Some name ->
      let symbol_prefix =
        if X86_proc.system = X86_proc.S_macosx then "_" else ""
      in
      Some (X86_proc.string_of_symbol symbol_prefix name)

(* Use addresses from permutation locations to find linear id layout. *)
let decode_item ~func ~locations fun_layout (l : Raw_layout.t) =
  let program_counter = Int64.(l.address + Int64.of_int l.offset) in
  let open Ocaml_locations in
  match decode_line locations ~program_counter func Linearid with
  | None -> fun_layout
  | Some (_, line) -> (
    match Map.add fun_layout ~key:l.position ~data:line with
    | `Duplicate ->
        failwithf "Cannot add linear_id %d at position %d in function %s"
          line l.position func ()
    | `Ok fun_layout ->
        if !verbose then
          Printf.printf "Added %s %d %d\n" func l.position line;
        fun_layout )

(* Split raw layout into functions and decode each one in turn. *)
let decode_layout_all locations permutation writer =
  let open Raw_layout in
  (* Resolve all addresses that need decoding, and cache them. *)
  let len = List.length permutation in
  let addresses = Caml.Hashtbl.create len in
  List.iter permutation ~f:(fun r ->
      let address = Int64.(r.address + of_int r.offset) in
      assert (not (Caml.Hashtbl.mem addresses address));
      Caml.Hashtbl.add addresses address () );
  Elf_locations.resolve_all locations addresses ~reset:true;
  (* Decode each function and record its layout. *)
  let rec decode_func_layout permutation layout =
    match permutation with
    | [] -> layout
    | hd :: tl -> (
        let func_start = hd.address in
        let func =
          Elf_locations.resolve_function_starting_at
            ~program_counter:func_start ~resolve_contents:false ~reset:false
            locations
        in
        match func with
        | None ->
            Report.log (sprintf "Not found function at 0x%Lx\n" func_start);
            decode_func_layout tl layout
        | Some func ->
            if !verbose then Printf.printf "Function %s\n" func;
            let l, rest =
              List.split_while permutation ~f:(fun r ->
                  r.address = func_start )
            in
            let fun_layout =
              List.fold l ~init:Int.Map.empty
                ~f:(decode_item ~func ~locations)
            in
            let labels = Int.Map.data fun_layout in
            let layout =
              if List.is_empty labels then (
                Report.log
                  (sprintf "Cannot decode layout of function %s at 0x%Lx\n"
                     func func_start);
                layout )
              else (
                (* Save decoded layout *)
                writer func labels;
                String.Map.add_exn layout ~key:func ~data:labels )
            in
            decode_func_layout rest layout )
  in
  decode_func_layout permutation String.Map.empty

module Func_layout = struct
  type t = string list

  let write t filename =
    if !verbose then printf "Writing function layout to %s\n" filename;
    let chan = Out_channel.create filename in
    List.iter t ~f:(fun name -> fprintf chan "%s\n" name);
    Out_channel.close chan

  let write_linker_script t filename =
    if !verbose then printf "Writing linker script hot to %s\n" filename;
    let chan = Out_channel.create filename in
    List.iter t ~f:(fun name -> fprintf chan "*(.text.%s)\n" name);
    Out_channel.close chan

  let read filename =
    if !verbose then printf "Reading function layout from %s\n" filename;
    let chan = In_channel.create filename in
    let t =
      In_channel.fold_lines chan ~init:[] ~f:(fun acc name -> name :: acc)
    in
    let t = List.rev t in
    if !verbose then (
      List.iter t ~f:(fun name -> printf "%s\n" name);
      if t = [] then printf "Empty function layout!\n"
      else printf "Layout size = %d" (List.length t) );
    t
end
