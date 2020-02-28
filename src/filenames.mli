(* Manage real and fake filenames.

   Special debug info encodes ids of items in an intermediate representation
   as line numbers in the corresponding compilation artifact file, which is
   actually binary not text so doesn't have line numbers. *)

type t =
  | Source
  | Linear

(* is filename of type t, i.e., does it have an expected extension. *)
val is_legal : t -> string -> bool

val compare : t -> expected:string -> actual:string -> bool

val make : t -> string -> string

val make_fdo : string -> string

val verbose : bool ref

val to_symbol : string -> string
(** Convert an ocaml compiler internal function name that may have special
    characters to an elf symbol name, using x86_64 dsl from ocaml compiler..
    This is the only machine dependent part.

    For example anonymous functions have square brackes and commas, and
    source can use ticks (') in functions names.

    Owee parser doesn't know how to handle /% that may appear in a function
    name, for example an Int operator. Assemblers reject other special
    characters, when we use function names as file names in DWARF debug info.
    It is also nice to avoid for per-function filenames such as (cfg in dot
    format) generated for debuging ocamlfdo itself. *)
