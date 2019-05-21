(**************************************************************************)
(*                                                                        *)
(*                                 OCamlFDO                               *)
(*                                                                        *)
(*                     Greta Yorsh, Jane Street Europe                    *)
(*                                                                        *)
(*   Copyright 2019 Jane Street Group LLC                                 *)
(*                                                                        *)
(*                     based on spacetime_lib                             *)
(*   Copyright (c) 2016 Leo White, Mark Shinwell                          *)
(*   https://github.com/lpw25/spacetime_lib                               *)
(*                                                                        *)
(*   All rights reserved.  This file is distributed under the terms of    *)
(*   the GNU Lesser General Public License version 2.1, with the          *)
(*   special exception on linking described in the file LICENSE.          *)
(*                                                                        *)
(**************************************************************************)

type t

val create : elf_executable:string -> t

val resolve : t -> program_counter:Int64.t -> (string * int) option

val function_at_pc : t -> program_counter:Int64.t -> string option

val resolve_function_starting_at
  : t
  -> program_counter:Int64.t
  -> reset:bool
  -> string option

val resolve_function_offsets
  : t
  -> program_counter:Int64.t
  -> int list
  -> reset:bool
  -> string option

(* Resolves debug info in one pass and caches the results for addresses. *)
val resolve_all : t -> Int64.t list -> reset:bool -> unit


val print_dwarf : t -> unit

