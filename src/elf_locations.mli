open Core

type t

val create : elf_executable:string -> t

val resolve_range :
  t -> start:Addr.t -> finish:Addr.t -> with_inverse:bool -> unit

val resolve_function_containing :
  t -> program_counter:Addr.t -> string Intervals.interval option

val find_functions : t -> (string, Addr.t option) Hashtbl.t -> unit

(* Resolves debug info in one pass and caches the results for addresses.
   Modifies the input hashtable inplace. *)
val resolve_all : t -> (Addr.t, Dbg.t option) Hashtbl.t -> unit

val print_dwarf : t -> unit

val to_address : t -> Dbg.t -> Addr.t option

(* reset caches that store addresses, but not the function symbols cache *)
val reset_cache : t -> unit

(* Iterate over the symbols and apply the function [f] to their names. It
   hides the details of symbol representation in the underlying ELF library. *)
val iter_symbols : t -> f:(string -> unit) -> unit

val verbose : bool ref
