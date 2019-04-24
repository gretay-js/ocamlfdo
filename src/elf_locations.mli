type t

val create : elf_executable:string -> t

val resolve : t -> program_counter:Int64.t -> (string * int) option

val function_at_pc : t -> program_counter:Int64.t -> string option

val resolve_function_at_pc : t -> program_counter:Int64.t -> string option

val print_dwarf : t -> unit

