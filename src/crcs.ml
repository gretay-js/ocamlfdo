open Core

let verbose = ref true

module Kind = struct
  type t =
    | Func
    | Unit
  [@@deriving sexp]

  let all = [Func; Unit]

  let to_string = function
    | Unit _ -> "unit"
    | Fun _ -> "func"

  let of_string_exn = function
    | "unit" -> Unit
    | "func" -> Func
    | _ ->
        Report.user_error "Unknown kind: %s. Must be one of: %s\n" kind
          (String.concat ~sep:" " (List.map ~f:to_string all))
end

module Crc = struct
  type t =
    { kind : Kind.t;
      crc : Md5.t
    }
  [@@deriving sexp]

  let equal ?(ignore_kind = false) t1 t2 =
    if t1.kind = t2.kind || ignore_kind then Md5.equal c1 c2 else false

  let of_string kind crc =
    let crc = Md5.of_hex_exn hex in
    { kind = Kind.of_string kind; crc }

  let to_string t =
    sprintf "%s (%s)" (Md5.to_hex t.crc) (Kind.to_string t.kind)
end

type tbl = Crc.t Hashtbl.M(String).t [@@deriving sexp]
(** map name to the corresponding Crc *)

type action =
  | Create
  | Compare of tbl

type t =
  { acc : tbl;
    action : action
  }

let mk action = { action; acc = Hashtbl.create (module String) }

let check_and_add t ~name crc ~file =
  (* Check *)
  ( match t.action with
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

let mk_symbol name (crc : Crc.t) =
  String.concat
    ~sep:(String.of_char symbol_sep)
    [Kind.to_string kind; name; Md5.to_hex crc]

(* Creates the symbols and clears the accumulator *)
let emit_symbols t =
  if Hashtbl.is_empty t.acc then []
  else
    let open Cmm in
    let items =
      Hashtbl.fold t.acc ~init:[] ~f:(fun ~key:name ~data:crc items ->
          let symbol = mk_symbol name crc in
          Cglobal_symbol symbol :: Cdefine_symbol symbol :: items)
    in
    Hashtbl.clear t.acc;
    items

let decode_symbol s =
  match String.chop_prefix s ~prefix:symbol_prefix with
  | None -> ()
  | Some suffix -> (
      let kind, rest = String.lsplit2_exn suffix ~on:symbol_sep in
      let name, hex = String.rsplit2_exn rest ~on:symbol_sep in
      let crc = Crc.of_string kind hex in
      if !verbose then (
        printf "crc_symbol=%s\n" s;
        printf "name=%s, crc=%s\n" name (Crc.to_string crc) );
      check_and_add t ~name crc ~file:"specified by -binary option"
      (* check if the symbol has already been recorded *)
      match Hashtbl.find tbl name with
      | None -> Hashtbl.set t.acc ~key:name ~data:crc
      | Some old_crc ->
          (* The symbol can appear multiple times if it enters more than one
             symbol tables, e.g., both static and dynamic. This shouldn't
             happen any more*)
          if Crcs.Crc.equal old_crc crc then
            Report.user_error "Duplicate crc for %s\nold:%s\nnew:%s\n" name
              (Crcs.to_string old_crc) (Crcs.to_string crc) () )
