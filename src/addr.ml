open Core
include Int64

(* (* it should be private but then deriving sexp and hash don't work and need
 *    to be reimplemented manually, and every place we use it needs a
 *    conversion, not only at creation time. hmm.. *)
 * type t = Int64.t [@@deriving compare, sexp, hash, equal] *)
