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
