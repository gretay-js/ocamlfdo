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
(* map name to the corresponding md5 *)
type tbl = Md5.t Hashtbl.M(String).t

type kind =
  | Create
  | Compare of tbl

type t = {
  acc : tbl;
  kind : kind;
}

let mk kind = { kind; acc = Hashtbl.create (module String) }

let check_and_add t ~name crc ~file =
  (* Check *)
  ( match t with
  | Create -> ()
  | Compare tbl ->
      Hashtbl.find_and_call tbl name
        ~if_found:(fun old_crc ->
          if not (Md5.equal old_crc crc) then
            failwithf
              "Linear IR for %s from file %s does not match the version of \
               this IR used for creating the profile.\n\
               old crc: %s\n\
               new crc: %s\n"
              name file (Md5.to_hex old_crc) (Md5.to_hex crc) ())
        ~if_not_found:(fun name ->
          failwithf
            "Linear IR for %s from file %s was not used for creatingthe \
             profile.\n\
             old crc: %s\n\
             new crc: %s\n") );

  (* Add to accumulator *)
  Hashtbl.update acc name ~f:(function
    | None -> Some crc
    | Some old_crc ->
        if Md5.equal old_crc crc then
          failwithf
            "Duplicate! Linear IR for %s from file %s has already been \
             processed.\n\
             crc: %s\n"
            name file (Md5.to_hex crc) ()
        else
          failwithf
            "Linear IR for %s from file %s processed earlier does not match.\n\
             old crc: %s\n\
             new crc: %s\n"
            name file (Md5.to_hex old_crc) (Md5.to_hex crc) ())

let add_unit t ~name crc ~file = check_and_add t ~name crc ~file

let add_fun t f ~file =
  let name = f.fun_name in
  let crc = Md5.digest_bytes (Marshal.to_bytes f) in
  check_and_add t ~name crc file

(* Symbols in the binary with a special naming sceme to communite crc of the
   linear IR it was compiled from. This is used to ensure that the profile
   is only applied to IR that it was obtained from.

   format: <crc_symbol_prefix><unit_name><crc_symbol_sep><crc_hex> example:
   caml_-_cRc_-_Ocamlfdo-123415810438DFE5 *)
let symbol_prefix = "caml___cRc___"

let symbol_sep = '_'

let mk_symbol name crc =
  sprintf "%s%s%c%s" crc_symbol_prefix name crc_symbol_sep (Md5.to_hex crc)

(* Creates the symbols and clears the accumulator *)
let emit_symbols t =
  if not (Hashtbl.is_empty t.acc) then (
    let open Cmm in
    Hashtbl.fold t.acc ~init:[] ~f:(fun ~key:name ~data:crc items ->
        let symbol = mk_symbol name crc in
        Cglobal_symbol symbol :: Cdefine_symbol symbol :: items);
    Hashtbl.reset t.acc )
