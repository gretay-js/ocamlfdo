open Core

type rel =
  { id : int;  (** Unique id of the containing function symbol *)
    offset : int  (** Offset from the start of the function *)
  }
[@@deriving compare, sexp, hash, equal, bin_io]

type t =
  { rel : rel option;
    dbg : int option  (** debug info: linearid *)
  }
[@@deriving sexp, compare, equal, bin_io]

val merge : t -> t -> t

val rename : t -> old2new:int Int.Table.t -> t
