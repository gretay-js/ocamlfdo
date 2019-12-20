open Core

type rel =
  { (* Unique id of the containing function symbol *)
    id : int;
    (* Offset from the start of the function *)
    offset : int;
    (* cfg label of the block containing this location *)
    label : Cfg_label.t option
  }
[@@deriving compare, sexp, hash, equal]

type t =
  { addr : Raw_addr.t;
    (* Raw address in the original binary *)
    rel : rel option;
    (* debug info: linearid *)
    dbg : int option
  }
[@@deriving sexp, compare, equal]

let rename t ~old2new =
  match t.rel with
  | None -> t
  | Some rel ->
      let rel = Some { rel with id = Hashtbl.find_exn old2new rel.id } in
      { t with rel }

let merge t1 t2 =
  let fail msg =
    Report.user_error
      !"Cannot merge locations %s:\n%{sexp:t}\n%{sexp:t}\n"
      msg t1 t2
  in
  if not (Raw_addr.equal t1.addr t2.addr) then fail "for different addresses";
  let rel =
    match (t1.rel, t2.rel) with
    | None, None -> None
    | None, _ | _, None -> fail ""
    | Some r1, Some r2 ->
        if not (r1.id = r2.id && r1.offset = r2.offset) then
          fail "with mismatched function ids or offsets";
        let label =
          Option.merge r1.label r2.label ~f:(fun l1 l2 ->
              if not (l1 = l2) then fail "with different cfg labels";
              l1)
        in
        Some { r1 with label }
  in
  let dbg =
    Option.merge t1.dbg t2.dbg ~f:(fun d1 d2 ->
        if not (d1 = d2) then fail "with mismatched debug info";
        d1)
  in
  { addr = t1.addr; rel; dbg }
