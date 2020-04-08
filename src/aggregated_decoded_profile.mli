open Core

type function_name = string

type t =
  { (* map raw addresses to locations *)
    addr2loc : Loc.t Raw_addr.Table.t;
    (* map func name to func id *)
    name2id : int String.Table.t;
    (* map func id to func info *)
    functions : Func.t Int.Table.t;
    (* map name of compilation unit or function to its md5 digest. Currently
       contains only crcs of linear IR. Not using Caml.Digest.t because it
       does not have sexp. Not using Core's Digest because digests generated
       by the compiler using Caml.Digest might disagree. *)
    crcs : Crcs.tbl;
    (* buildid of the executable, if known *)
    mutable buildid : string option
  }
[@@deriving sexp, bin_io]

val create :
  Elf_locations.t ->
  Aggregated_perf_profile.t ->
  crc_config:Crcs.Config.t ->
  t

val read : string -> t

val write : t -> string -> unit

val write_bin : t -> string -> unit

val read_bin : string -> t

val to_sexp : string -> unit
(** read profile from file in bin format and print it to stdout as sexp *)

val of_sexp : input_filename:string -> output_filename:string -> unit
(** Read profile from file where it was saved as sexp and save it to output
    file in binary format. Useful for manually editting profiles for debug. *)

val id2name :
  t -> (int, function_name list, Core.Int.comparator_witness) Core.Map.t

val all_functions : t -> function_name list
(** all functions, ordered by IDs, not execution counts *)

val sorted_functions : t -> function_name list
(** all functions, in descending order of execution counts *)

val sorted_functions_with_counts : t -> (function_name * Execount.t) list
(** all functions, in descending order of execution counts, paired with the
    counts. *)

val print_sorted_functions_with_counts : t -> unit

val trim : t -> keep:(function_name -> bool) -> unit
(** Trim the profile in place, by keeping information only about functions
    for which [keep] returns true. *)

module Merge : Merge.Algo with type profile = t

val verbose : bool ref
