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
  { addr : Addr.t;
    (* Raw address in the original binary *)
    rel : rel option;
    (* Containing function info and relative offset *)
    dbg : Dbg.t option
  }
[@@deriving sexp, compare, equal]
