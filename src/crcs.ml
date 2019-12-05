open Core

let verbose = ref true

module Crc = struct
  type t =
    | Func of Md5.t
    | Unit of Md5.t

  let equals ?(ignore_kind = false) t1 t2 =
    match (t1, t2) with
    | Func c1, Func c2 | Unit c1, Unit c2 -> Md5.equal c1 c2
    | Func c1, Unit c2 | Unit c1, Func c2 ->
        if ignore_type then Md5.equal c1 c2 else false

  let to_string t =
    let kind =
      match t with
      | Unit c -> "compilation unit"
      | Func c -> "function"
    in
    sprintf "%s (%s)" kind (Md5.to_hex c)
end

type tbl = Crc.t Hashtbl.M(String).t

type kind =
  | Create
  | Compare of tbl

type t =
  { acc : tbl;
    kind : kind
  }

let mk_tbl () = Hashtbl.create (module String)

let mk kind = { kind; acc = Hashtbl.create (module String) }

let check_and_add t ~name crc ~file =
  (* Check *)
  ( match t.kind with
  | Create -> ()
  | Compare tbl ->
      Hashtbl.find_and_call tbl name
        ~if_found:(fun old_crc ->
          if not (Crc.equal old_crc crc) then
            Report.user_error
              "Linear IR for %s from file %s does not match the version of \
               this IR used for creating the profiled binary.\n\
               old crc: %s\n\
               new crc: %s\n"
              name file (Crc.to_string old_crc) (Crc.to_string crc))
        ~if_not_found:(fun name ->
          if !verbose then
            Printf.printf
              "Linear IR for %s from file %s was not used for creating the \
               profiled binary.\n\
               new crc: %s" name file (Crc.to_string crc)) );

  (* Add to accumulator *)
  Hashtbl.update t.acc name ~f:(function
    | None -> crc
    | Some old_crc ->
        if Crc.equal old_crc crc then
          Report.user_error
            "Duplicate! Linear IR for %s from file %s has already been \
             processed.\n\
            \                crc: %s" name file (Crc.to_string crc) ()
        else
          Report.user_error
            "Linear IR for %s from file %s processed earlier does not match.\n\
            \             old crc: %s\n\
            \             new crc: %s" name file (Crc.to_string old_crc)
            (Crc.to_string crc) ())

let add_unit t ~name crc ~file = check_and_add t ~name (Unit crc) ~file

let add_fun t f ~file =
  let name = f.Linear.fun_name in
  let crc = Md5.digest_bytes (Marshal.to_bytes f []) in
  check_and_add t ~name (Func crc) ~file

(* Symbols in the binary with a special naming sceme to communite crc of the
   linear IR it was compiled from. This is used to ensure that the profile is
   only applied to IR that it was obtained from.

   format: <crc_symbol_prefix><unit_name><crc_symbol_sep><crc_hex> example:
   caml_-_cRc_-_Ocamlfdo-123415810438DFE5 *)
let symbol_prefix = "caml___cRc___"

let symbol_sep = '_'

let mk_symbol name crc =
  sprintf "%s%s%c%s" symbol_prefix name symbol_sep (Md5.to_hex crc)

(* Creates the symbols and clears the accumulator *)
let emit_symbols t =
  if Hashtbl.is_empty t.acc then []
  else
    let open Cmm in
    let items =
      Hashtbl.fold t.acc ~init:[] ~f:(fun ~key:name ~data items ->
          let crc =
            match data with
            | Unit crc | Func crc -> crc
          in
          let symbol = mk_symbol name crc in
          Cglobal_symbol symbol :: Cdefine_symbol symbol :: items)
    in
    Hashtbl.clear t.acc;
    items
