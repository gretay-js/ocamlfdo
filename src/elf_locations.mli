open Core

type t

type function_sym = private
  { name : string
  ; local : bool
  }

val create : elf_executable:string -> t

val resolve_range
  :  t
  -> start:Raw_addr.t
  -> finish:Raw_addr.t
  -> with_inverse:bool
  -> unit

val resolve_function_containing
  :  t
  -> program_counter:Raw_addr.t
  -> function_sym Intervals.interval option

(* Resolves debug info in one pass and caches the results for addresses.
   Modifies the input hashtable inplace. *)

val resolve_all : t -> (Raw_addr.t, Dbg.t option) Hashtbl.t -> unit
val print_dwarf : t -> unit
val to_address : t -> Dbg.t -> Raw_addr.t option

(** Iterate over the symbols and apply the function [f] to their names. It
    hides the details of symbol representation in the underlying ELF library.
    When [func] is true, apply [f] to function symbols. When [data] is true,
    apply [f] to all other (not function) symbols. *)
val iter_symbols
  :  t
  -> func:bool
  -> data:bool
  -> f:(string -> Raw_addr.t -> unit)
  -> unit

val verbose : bool ref
