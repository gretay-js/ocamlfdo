open Core

let verbose = ref true

module Kind = struct
  type t =
    | Func
    | Unit
  [@@deriving sexp, equal]

  let all = [Func; Unit]

  let to_string = function
    | Unit -> "u"
    | Func -> "f"

  exception Parse_error of string

  let of_string_exn s =
    match List.find all ~f:(fun k -> String.equal s (to_string k)) with
    | Some k -> k
    | None ->
        let msg =
          sprintf "Unknown kind: %s. Kind must be one of: %s\n" s
            (String.concat ~sep:" " (List.map ~f:to_string all))
        in
        raise (Parse_error msg)
end

module Crc = struct
  type t =
    { kind : Kind.t;
      crc : Md5.t
    }
  [@@deriving sexp, equal]

  let of_string kind hex =
    let crc = Md5.of_hex_exn hex in
    { kind = Kind.of_string_exn kind; crc }
end

type tbl = Crc.t Hashtbl.M(String).t [@@deriving sexp]
(** map name to the corresponding Crc *)

type action =
  | Create
  | Compare of tbl

type config =
  { unit : bool;
    func : bool
  }

type t =
  { acc : tbl;
    action : action;
    config : config
  }

let tbl t = t.acc

let mk action config =
  { action; acc = Hashtbl.create (module String); config }

let check_and_add ?(error_on_duplicate = true) t ~name crc ~file =
  (* Check *)
  ( match t.action with
  | Create -> ()
  | Compare tbl ->
      Hashtbl.find_and_call tbl name
        ~if_found:(fun old_crc ->
          if not (Crc.equal old_crc crc) then
            Report.user_error
              !"Linear IR for %s from file %s does not match the version of \
                this IR used for creating the profiled binary.\n\
                old crc: %{sexp:Crc.t}\n\
                new crc: %{sexp:Crc.t}\n"
              name file old_crc crc)
        ~if_not_found:(fun name ->
          if !verbose then
            Printf.printf
              !"Linear IR for %s from file %s was not used for creating the \
                profiled binary.\n\
                new crc: %{sexp:Crc.t}\n"
              name file crc) );

  (* Add to accumulator *)
  let dup old_crc =
    if Crc.equal old_crc crc then
      if error_on_duplicate then
        Report.user_error
          !"Duplicate! Linear IR for %s from file %s has already been \
            processed.\n\
            crc: %{sexp:Crc.t}"
          name file crc
      else (
        if !verbose then
          Printf.printf
            !"Duplicate! Linear IR for %s from file %s has already been \
              processed.\n\
              crc: %{sexp:Crc.t}\n"
            name file crc;
        old_crc )
    else
      Report.user_error
        !"Linear IR for %s from file %s processed earlier does not match.\n\
          old crc: %{sexp:Crc.t}\n\
          new crc: %{sexp:Crc.t}"
        name file old_crc crc
  in
  Hashtbl.update t.acc name ~f:(function
    | None -> crc
    | Some old_crc -> dup old_crc)

let add_unit t ~name crc ~file =
  if t.config.unit then check_and_add t ~name { kind = Unit; crc } ~file

let add_fun t f ~file =
  if t.config.func then
    let name = f.Linear.fun_name in
    let crc = Md5.digest_bytes (Marshal.to_bytes f []) in
    check_and_add t ~name { kind = Func; crc } ~file

(* Symbols in the binary with a special naming sceme to communite crc of the
   linear IR it was compiled from. This is used to ensure that the profile is
   only applied to IR that it was obtained from.

   format: <crc_symbol_prefix><unit_name><crc_symbol_sep><crc_hex> example:
   caml_-_cRc_-_Ocamlfdo-123415810438DFE5 *)
let symbol_prefix = "caml___cRc___"

let symbol_sep = '_'

let symbol_sep_str = String.of_char symbol_sep

let mk_symbol name (crc : Crc.t) =
  String.concat
    [ symbol_prefix;
      Kind.to_string crc.kind;
      symbol_sep_str;
      name;
      symbol_sep_str;
      Md5.to_hex crc.crc ]

(* Creates the symbol names and clears the accumulator *)
let encode t =
  if Hashtbl.is_empty t.acc || not (t.config.unit || t.config.func) then []
  else
    let items =
      Hashtbl.to_alist t.acc
      |> List.map ~f:(fun (name, crc) -> mk_symbol name crc)
    in
    Hashtbl.clear t.acc;
    items

(* decodes the symbol name and adds it to [t] *)
let decode_and_add t s =
  match String.chop_prefix s ~prefix:symbol_prefix with
  | None -> ()
  | Some suffix ->
      let kind, rest = String.lsplit2_exn suffix ~on:symbol_sep in
      let name, hex = String.rsplit2_exn rest ~on:symbol_sep in
      let crc =
        try Crc.of_string kind hex
        with Kind.Parse_error msg ->
          Report.user_error ~hint:(Some Report.Hint.Old_profile)
            "Cannot decode crc symbol %s\n%s" s msg
      in
      if !verbose then (
        printf "crc_symbol=%s\n" s;
        printf !"name=%s, crc=%{sexp:Crc.t}\n" name crc );
      (* check if the symbol has already been recorded *)
      (* CR-soon gyorsh: owee returns duplicate symbols, even though the
         symbol table has only one entry in the symbol table, because of the
         way owee iterates over symbols using interval tree. *)
      check_and_add ~error_on_duplicate:false t ~name crc
        ~file:"specified by -binary option"

let merge_into ~src ~dst config =
  let merge_crcs ~key (a : Crc.t) b =
    match b with
    | None ->
        (* If one of the CRC is missing, we can't check it, so require
           -no-md5 option *)
        let fail =
          (Kind.equal a.kind Unit && config.unit)
          || (Kind.equal a.kind Func && config.func)
        in
        if fail then
          Report.user_error
            !"Merge aggregated decoded profiles: one profile is missing crc \
              for %s,\n\
              another profile has\n\
              %{sexp:Crc.t}\n"
            key a
        else Hashtbl.Set_to a
    | Some b ->
        if Crc.equal a b then Hashtbl.Set_to b
        else
          let fail =
            (config.unit || config.func)
            && ( (not (Kind.equal a.kind b.kind))
               || (Kind.equal a.kind Unit && config.unit)
               || (Kind.equal a.kind Func && config.func) )
          in
          if fail then
            Report.user_error
              !"Merge aggregated decoded profiles: mismatched crcs for %s:\n\
                %{sexp:Crc.t}\n\
                %{sexp:Crc.t}\n"
              key a b
          else (
            if !verbose then
              Printf.printf
                !"Merge aggregated decoded profiles: mismatched crcs for %s:\n\
                  %{sexp:Crc.t}\n\
                  %{sexp:Crc.t}\n"
                key a b;
            Hashtbl.Remove )
  in

  Hashtbl.merge_into ~src ~dst ~f:merge_crcs
