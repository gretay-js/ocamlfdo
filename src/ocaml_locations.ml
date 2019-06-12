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
(* This is a little wrapper around Elf_locations to manage the file names
   specific to ocaml source and compiler's IR. *)
open Core

let verbose = ref true

type t =
  | Source
  | Linearid

(* CR gyorsh: what are all the source file extensions we should support? *)
let suffix = function
  | Linearid -> [ ".linear" ]
  | Source -> [ ".ml"; ".mli"; ".c"; ".h" ]

let decode_line locations ~program_counter func t =
  match Elf_locations.resolve_from_cache ~program_counter locations with
  | None ->
      if !verbose then
        Printf.printf "Elf location NOT FOUND at 0x%Lx\n" program_counter;
      None
  | Some (file, line) -> (
      if !verbose then Printf.printf "%s:%d\n" file line;
      let suffixes = suffix t in
      (* Check that the filename is supported. *)
      match
        List.find ~f:(fun s -> String.is_suffix file ~suffix:s) suffixes
      with
      | None ->
          Report.log (sprintf "Ignoring %s in %s\n" func file);
          None
      | Some suffix -> (
        match t with
        | Source -> Some (file, line)
        | Linearid -> (
          (* Checks that debug info is relative to the input function, i.e.,
             the name of the "file" matches the name of the function. We
             check that the function symbol name from the binary matches the
             function name encoded as filename into our special dwarf info. *)
          match String.chop_suffix file ~suffix with
          | None ->
              Report.log (sprintf "Ignoring %s in %s\n" func file);
              None
          | Some func_name_dwarf ->
              if func_name_dwarf = func then Some (file, line)
              else
                failwithf "func_name_dwarf = %s func = %s\n" func_name_dwarf
                  func () ) ) )

let to_address locations name line t =
  let file =
    match t with
    | Source -> name
    | Linearid ->
        let suffix = List.hd_exn (suffix t) in
        name ^ suffix
  in
  Elf_locations.to_address locations file line
