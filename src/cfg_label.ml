(* It should be Cfg.label, but we can't add sexp to Cfg, because the intent
   is to eventually integrate Cfg in the compiler, which doesn't currently
   use sexp. We use sexp to convert to/from file. *)
open Core

type t = int [@@deriving compare, sexp, hash, equal, bin_io]
