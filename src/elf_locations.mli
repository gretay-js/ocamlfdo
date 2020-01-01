open Core

type t

val create : elf_executable:string -> t

val resolve_range :
  t -> start:Raw_addr.t -> finish:Raw_addr.t -> with_inverse:bool -> unit

val resolve_function_containing :
  t -> program_counter:Raw_addr.t -> string Intervals.interval option

val find_functions : t -> (string, Raw_addr.t option) Hashtbl.t -> unit

(* Resolves debug info in one pass and caches the results for addresses.
   Modifies the input hashtable inplace. *)
val resolve_all : t -> (Raw_addr.t, Dbg.t option) Hashtbl.t -> unit

val print_dwarf : t -> unit

val to_address : t -> Dbg.t -> Raw_addr.t option

(* reset caches that store addresses, but not the function symbols cache *)
val reset_cache : t -> unit

(* Iterate over the symbols and apply the function [f] to their names. It
   hides the details of symbol representation in the underlying ELF library. *)
val iter_symbols : t -> f:(string -> Raw_addr.t -> unit) -> unit

val verbose : bool ref
