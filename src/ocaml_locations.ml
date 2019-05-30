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

type t =
  | Source
  | Linearid

let suffix = function
  | Source -> ".ml"
  | Linearid -> ".linear"

let decode_line locations program_counter func t =
  match Elf_locations.resolve_function_containing ~program_counter locations with
  | None ->
    if !verbose then
      Printf.printf "Elf location NOT FOUND at 0x%Lx\n" program_counter;
    None
  | Some (file,line) ->
    if !verbose then
      Printf.printf "%s:%d\n" file line;
    (* Check that the func symbol name from the binary where
       permutation comes from matches the function name encoded
       as filename into our special dwarf info. *)
    let suffix = suffix t in
    match String.chop_suffix file ~suffix with
    | None ->
      Report.log (sprintf "Ignoring %s in %s\n" func file);
      None
    | Some func_name_dwarf ->
      if func_name_dwarf = func then begin
        Some (file,line)
      end else
        failwithf "func_name_dwarf = %s func = %s\n"
          func_name_dwarf func ()
