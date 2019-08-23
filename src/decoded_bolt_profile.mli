(**************************************************************************)
(*                                                                        *)
(*                                 OCamlFDO                               *)
(*                                                                        *)
(*                     Greta Yorsh, Jane Street Europe                    *)
(*                                                                        *)
(*   Copyright 2019 Jane Street Group LLC                                 *)
(*                                                                        *)
(*   All rights reserved.  This file is distributed under the terms of    *)
(*   the GNU Lesser General Public License version 2.1, with the          *)
(*   special exception on linking described in the file LICENSE.          *)
(*                                                                        *)
(**************************************************************************)
type t

val create : Elf_locations.t -> filename:string -> t

val export : t -> Aggregated_perf_profile.t

val save :
  Aggregated_decoded_profile.t ->
  Aggregated_perf_profile.t ->
  filename:string ->
  unit

val save_fallthrough :
  Aggregated_decoded_profile.t -> filename:string -> unit

val write : t -> filename:string -> unit

val verbose : bool ref
