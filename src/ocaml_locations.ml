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

type t = Source | Linearid

(* CR gyorsh: what are all the source file extensions we should support? *)
let suffix = function
  | Linearid -> [".linear"]
  | Source -> [".ml"; ".mli"; ".c"; ".h"]

let unique_suffix t = match suffix t with [s] -> s | _ -> assert false

(* Checks that debug info is relative to the input function, i.e., the name
   of the "file" matches the name of the function. *)
let decode_line_ir locations ~program_counter func t =
  match Elf_locations.resolve ~program_counter locations with
  | None ->
      if !verbose then
        Printf.printf "Elf location NOT FOUND at 0x%Lx\n" program_counter ;
      None
  | Some (file, line) -> (
      if !verbose then Printf.printf "%s:%d\n" file line ;
      (* Check that the func symbol name from the binary where permutation
         comes from matches the function name encoded as filename into our
         special dwarf info. *)
      let suffix = unique_suffix t in
      match String.chop_suffix file ~suffix with
      | None ->
          Report.log (sprintf "Ignoring %s in %s\n" func file) ;
          None
      | Some func_name_dwarf ->
          if func_name_dwarf = func then Some (file, line)
          else
            failwithf "func_name_dwarf = %s func = %s\n" func_name_dwarf
              func () )

let decode_line locations ~program_counter func t =
  match Elf_locations.resolve ~program_counter locations with
  | None ->
      if !verbose then
        Printf.printf "Elf location NOT FOUND at 0x%Lx\n" program_counter ;
      None
  | Some (file, line) -> (
      if !verbose then Printf.printf "%s:%d\n" file line ;
      (* Check that the filename is supported. *)
      let suffixes = suffix t in
      match
        List.find ~f:(fun s -> String.is_suffix file ~suffix:s) suffixes
      with
      | None ->
          Report.log (sprintf "Ignoring %s in %s\n" func file) ;
          None
      | Some _ -> Some (file, line) )

let decode_line locations ~program_counter func t =
  match t with
  | Linearid -> decode_line_ir locations ~program_counter func t
  | Source -> decode_line locations ~program_counter func t
