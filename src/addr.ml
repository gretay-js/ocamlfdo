open Core

(* it should be private but then deriving sexp and hash don't work and need
   to be reimplemented manually, and every place we use it needs a
   conversion, not only at creation time. hmm.. *)
type t = int64 [@@deriving compare, sexp, hash, equal]
