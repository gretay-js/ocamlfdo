open Core

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

val id2name : t -> (int, string list, Core.Int.comparator_witness) Core.Map.t

val trim_functions : t -> cutoff:Trim.t -> unit
(** Trim the profile in place, by removing functions that don't make the
    cutoff. *)

val all_functions : t -> string list
(** all functions, ordered by IDs, not execution counts. *)

val top_functions : t -> string list
(** all functions, in descending order of execution counts. *)

val print_sorted_functions_with_counts : t -> unit

val print_stats : t -> unit
(** print size of compiler components, based on sexp representation, and
    various statistics about the content of the profile. Useful for
    understanding the cause of very large profiles and how to best trim them. *)

module Merge : Merge.Algo with type profile = t

val verbose : bool ref
