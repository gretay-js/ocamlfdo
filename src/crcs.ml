open Core

let verbose = ref true

module Kind = struct
  type t =
    | Func
    | Unit
  [@@deriving sexp, equal, bin_io]

  let all = [Func; Unit]

  let to_string = function
    | Unit -> "u"
    | Func -> "f"

  let to_string_hum = function
    | Unit -> "compilation unit"
    | Func -> "function"

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
  [@@deriving sexp, equal, bin_io]

  let of_string kind hex =
    let crc = Md5.of_hex_exn hex in
    { kind = Kind.of_string_exn kind; crc }
end

type tbl = Crc.t String.Table.t [@@deriving sexp, bin_io]
(** map name to the corresponding Crc *)

type action =
  | Create
  | Compare of tbl

exception Near_match of string list

module On_error = struct
  type t =
    | Fail
    | Skip
    | Use_anyway
  [@@deriving enumerate, equal]

  let default = Fail

  let to_string = function
    | Fail -> "fail"
    | Use_anyway -> "use-anyway"
    | Skip -> "skip"
end

module Level = struct
  type t =
    { mutable mismatch : int;
      mutable missing : int;
      mutable total : int;
      kind : Kind.t;
      enabled : bool;
      on_mismatch : On_error.t;
      on_missing : On_error.t
    }

  let mk ~enabled kind ~on_missing ~on_mismatch =
    { mismatch = 0;
      missing = 0;
      total = 0;
      kind;
      enabled;
      on_missing;
      on_mismatch
    }

  let inc_missing t =
    t.total <- t.total + 1;
    t.missing <- t.missing + 1

  let inc_mismatch t =
    t.total <- t.total + 1;
    t.mismatch <- t.mismatch + 1

  let record t = t.total <- t.total + 1

  let report_field t v msg =
    if v > 0 then
      Report.logf "%s md5 in %d %s%s out of %d (%.3f%%)\n" msg v
        (Kind.to_string_hum t.kind)
        (if v = 1 then "" else "s")
        t.total
        (Report.percent v t.total)

  let report t =
    report_field t t.missing "Missing";
    report_field t t.mismatch "Mismatched"
end

module Config = struct
  (* CR-soon gyorsh: separate on_mismatch to be per unit and per function?
     Per function does not superceed per unit, if only data part of the unit
     changed, then unit crc will be different, all but func crc will be the
     same. *)
  (* CR-soon gyorsh: this is ugly, everything appears twice, how to avoid it? *)
  type t =
    { unit : Level.t;
      func : Level.t;
      ignore_dbg : bool
    }

  let mk ~on_mismatch ~on_missing ~func ~unit ~ignore_dbg =
    { unit = Level.mk ~enabled:unit Unit ~on_missing ~on_mismatch;
      func = Level.mk ~enabled:func Func ~on_missing ~on_mismatch;
      ignore_dbg
    }

  let report t =
    Level.report t.unit;
    Level.report t.func

  let record t = function
    | Kind.Unit -> Level.record t.unit
    | Kind.Func -> Level.record t.func

  (* [skip] is the result in the case of skip *)
  let handle (on_error : On_error.t) ~msg ~skip =
    match on_error with
    | Fail -> Report.user_error "%s\n" msg
    | Use_anyway ->
        if !verbose then Printf.printf "Use anyway.\n";
        true
    | Skip ->
        if !verbose then
          if skip then Printf.printf "Skipping.\n"
          else Printf.printf "Not skipping units.\n";
        skip

  (* result in the case of skip depends on both unit and func settings. *)
  let handle_mismatch t ~msg (kind : Kind.t) =
    match kind with
    | Unit ->
        Level.inc_mismatch t.unit;
        handle t.unit.on_mismatch ~msg ~skip:t.func.enabled
    | Func ->
        Level.inc_mismatch t.func;
        handle t.unit.on_mismatch ~msg ~skip:false

  let handle_missing t ~msg (kind : Kind.t) ~near_matches =
    match kind with
    | Unit ->
        Level.inc_missing t.unit;
        handle t.unit.on_missing ~msg ~skip:t.func.enabled
    | Func -> (
        Level.inc_missing t.func;
        match t.func.on_missing with
        | Fail -> Report.user_error "%s\n" msg
        | Use_anyway ->
            if List.is_empty near_matches then true
            else (
              if !verbose then
                Printf.printf
                  !"Found %d near matches:\n%{sexp:string list}.\n"
                  (List.length near_matches)
                  near_matches;
              raise (Near_match near_matches) )
        | Skip -> false )
end

type t =
  { acc : tbl;
    action : action;
    config : Config.t
  }

let tbl t = t.acc

let mk action config = { action; acc = String.Table.create (); config }

let add ?(error_on_duplicate = true) tbl ~name crc ~file =
  (* Add to accumulator *)
  let dup old_crc =
    if Crc.equal old_crc crc then
      let msg =
        Printf.sprintf
          !"Duplicate! Linear IR for %s from file %s has already been \
            processed.\n\
            crc: %{sexp:Crc.t}"
          name file crc
      in
      if error_on_duplicate then Report.user_error "%s\n" msg
      else (
        if !verbose then print_endline msg;
        old_crc )
    else
      Report.user_error
        !"Linear IR for %s from file %s processed earlier does not match.\n\
          old crc: %{sexp:Crc.t}\n\
          new crc: %{sexp:Crc.t}"
        name file old_crc crc
  in
  Hashtbl.update tbl name ~f:(function
    | None -> crc
    | Some old_crc -> dup old_crc)

let check tbl config ~name crc ~file =
  (* Check and raise if mismatch or missing in the reference tbl. *)
  Hashtbl.find_and_call tbl name
    ~if_found:(fun old_crc ->
      if Crc.equal old_crc crc then (
        Config.record config crc.kind;
        true )
      else
        let msg =
          sprintf
            !"Linear IR for %s from file %s does not match the IR used for \
              creating the profiled binary.\n\
              old crc: %{sexp:Crc.t}\n\
              new crc: %{sexp:Crc.t}\n"
            name file old_crc crc
        in
        if !verbose then print_endline msg;
        Config.handle_mismatch config ~msg crc.kind)
    ~if_not_found:(fun name ->
      (* If profiled binary didn't emit any crc or if the code we are
         compiling was not linked into the original binary, e.g., new code or
         was not needed, or function name changed. This should not be a
         failure at this point, as there may be no profile for this function
         at all, so it could claimed that the crc is not relevant. *)
      let near_matches =
        match crc.kind with
        | Unit -> []
        | Func ->
            let get_prefix s = String.rstrip ~drop:Char.is_digit s in
            let prefix = get_prefix name in
            let match_prefix s = String.equal (get_prefix s) prefix in
            Hashtbl.filter_keys tbl ~f:match_prefix |> Hashtbl.keys
      in
      let msg =
        sprintf
          !"Linear IR for %s from file %s was not used for creating the \
            profiled binary.\n\
            new crc: %{sexp:Crc.t}\n\n\
            near matches:\n\
            %{sexp:string List.t}"
          name file crc near_matches
      in
      if !verbose then print_endline msg;
      Config.handle_missing config ~msg crc.kind ~near_matches)

let check_and_add ?error_on_duplicate t ~name crc ~file =
  (* The order is important here: always add, so that the optimized binaries
     have correct, up-to-date, crcs. Then check and return true if and only
     if the check passes. *)
  add ?error_on_duplicate t.acc ~name crc ~file;
  match t.action with
  | Create -> true
  | Compare tbl -> check tbl t.config ~name crc ~file

(* CR-soon gyorsh: do we need crc of data? *)
let add_unit t (ui : Linear_format.linear_unit_info) ~hex ~file =
  if t.config.unit.enabled then
    let name = ui.unit_name in
    let crc =
      if t.config.ignore_dbg then
        let ui = Remove_dbg.unit ui in
        Md5.digest_bytes (Marshal.to_bytes ui [])
      else Md5.of_hex_exn hex
    in
    check_and_add t ~name { kind = Unit; crc } ~file
  else true

let add_fun t f ~file =
  if t.config.func.enabled then
    let name = Filenames.to_symbol f.Linear.fun_name in
    let f = if t.config.ignore_dbg then Remove_dbg.fundecl f else f in
    let crc = Md5.digest_bytes (Marshal.to_bytes f []) in
    check_and_add t ~name { kind = Func; crc } ~file
  else true

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
  if
    Hashtbl.is_empty t.acc
    || not (t.config.unit.enabled || t.config.func.enabled)
  then []
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
      assert (
        check_and_add ~error_on_duplicate:false t ~name crc
          ~file:"specified by -binary option" )

let merge_into ~src ~dst (config : Config.t) =
  let merge_crcs ~key (a : Crc.t) b =
    match b with
    | None ->
        (* If one of the CRC is missing, we can't check it, so require
           "-on-md5-mismatch skip" option. It is a mismatch only if the
           appropriate config kind is enabled. *)
        let enabled, on_missing =
          match a.kind with
          | Unit -> (config.unit.enabled, config.unit.on_missing)
          | Func -> (config.func.enabled, config.func.on_missing)
        in
        if enabled then (
          let msg =
            sprintf
              !"Merge aggregated decoded profiles: one profile is missing \
                crc for %s,\n\
                another profile has\n\
                %{sexp:Crc.t}\n"
              key a
          in
          match on_missing with
          | Fail -> Report.user_error "%s\n" msg
          | Skip ->
              if !verbose then print_endline msg;
              Hashtbl.Remove
          | Use_anyway ->
              if !verbose then print_endline msg;
              Hashtbl.Set_to a )
        else Hashtbl.Set_to a
    | Some b -> (
        if Crc.equal a b then Hashtbl.Set_to b
        else
          let msg =
            sprintf
              !"Merge aggregated decoded profiles: mismatched crcs for %s:\n\
                %{sexp:Crc.t}\n\
                %{sexp:Crc.t}\n"
              key a b
          in

          let f (c : Crc.t) =
            match c.kind with
            | Unit -> (config.unit.enabled, config.unit.on_mismatch)
            | Func -> (config.func.enabled, config.func.on_mismatch)
          in
          let enabled, on_mismatch =
            if Kind.equal a.kind b.kind then f a
            else
              let e_a, o_a = f a in
              let e_b, o_b = f b in
              assert (On_error.equal o_a o_b);
              let enabled = e_a || e_b in
              (enabled, if enabled then o_a else Fail)
          in
          if not enabled then Hashtbl.Remove
          else
            match on_mismatch with
            | Fail -> Report.user_error "%s\n" msg
            | Skip ->
                if !verbose then print_endline msg;
                Hashtbl.Remove
            | Use_anyway ->
                if !verbose then (
                  print_endline msg;
                  print_endline
                    "Removing anyway.\n\
                     Otherwise the result depends on the order in which \
                     profile files are given to merge." );
                Hashtbl.Remove )
  in
  Hashtbl.merge_into ~src ~dst ~f:merge_crcs
