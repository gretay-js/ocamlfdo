open Core
open Dbg
open Loc
open Func
module Cfg_with_layout = Ocamlcfg.Cfg_with_layout
module Cfg = Ocamlcfg.Cfg

let verbose = ref true

let ignore_local_dup = ref false

let _magic_number = "ocamlfdo001"

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

let mk size crcs buildid =
  { addr2loc = Raw_addr.Table.create ~size ();
    name2id = String.Table.create ();
    functions = Int.Table.create ();
    crcs;
    buildid
  }

let get_func t addr =
  match Raw_addr.Table.find t.addr2loc addr with
  | None ->
      if !verbose then
        printf "Not found any cached location for address 0x%Lx\n" addr;
      None
  | Some loc -> (
      match loc.rel with
      | None -> None
      | Some rel ->
          let id = rel.id in
          let func = Int.Table.find_exn t.functions id in
          Some func )

(* Source or target addr of this branch does not belong to the binary and
   usually to_addr cannot be target of jump because it is not a return from a
   call instruction and not an entry function. Could it be a context switch? *)
let track_branch (from_addr, to_addr) =
  if Raw_addr.(from_addr < 0L) || Raw_addr.(to_addr < 0L) then (
    if !verbose then
      printf "ignore branch 0x%Lx -> 0x%Lx (addr < 0)\n" from_addr to_addr;
    false )
  else true

(* Partition aggregated_perf to functions and calculate total execution
   counts of each function. Total execution count of a function is determined
   from the execution counts of samples contained in this function. It uses
   LBR info: if a branch source or target is contained in the function, it
   contributes to execution count of the function. It does not use the CFG.
   In particular, it does not count instructions that can be traced using
   LBR. The advantage is that we can compute it for non-OCaml functions. *)
let create_func_execounts t (agg : Aggregated_perf_profile.t) =
  Raw_addr.Table.iteri agg.instructions ~f:(fun ~key ~data ->
      match get_func t key with
      | None -> ()
      | Some func ->
          func.count <- Execount.(func.count + data);
          Raw_addr.Table.add_exn func.agg.instructions ~key ~data);
  let process (from_addr, to_addr) update =
    match (get_func t from_addr, get_func t to_addr) with
    | None, None -> ()
    | None, Some to_func -> update to_func
    | Some from_func, None -> update from_func
    | Some from_func, Some to_func ->
        if from_func.id = to_func.id then update to_func
        else (
          (* interprocedural branch: add to both functions *)
          update from_func;
          update to_func )
  in
  Raw_addr_pair.Table.iteri agg.branches ~f:(fun ~key ~data ->
      let mispredicts =
        Option.value
          (Raw_addr_pair.Table.find agg.mispredicts key)
          ~default:0L
      in
      let update_br func =
        func.count <- Execount.(func.count + data);
        if track_branch key then (
          Raw_addr_pair.Table.add_exn func.agg.branches ~key ~data;
          if Execount.(mispredicts > 0L) then
            Raw_addr_pair.Table.add_exn func.agg.mispredicts ~key
              ~data:mispredicts )
      in
      process key update_br);
  Raw_addr_pair.Table.iteri agg.traces ~f:(fun ~key ~data ->
      (* traces don't contribute to func's total count because it is
         accounted for in branches. *)
      let update_tr func =
        if track_branch key then
          Raw_addr_pair.Table.add_exn func.agg.traces ~key ~data
      in
      process key update_tr)

(* Find or add the function and return its id *)
let get_or_add_func_id t (i : Elf_locations.function_sym Intervals.interval)
    =
  let start = i.l in
  let finish = i.r in
  let name = i.v.name in
  match String.Table.find t.name2id name with
  | None ->
      let id = Int.Table.length t.functions in
      let func = Func.mk ~id ~start ~finish in
      Int.Table.add_exn t.functions ~key:id ~data:func;
      String.Table.add_exn t.name2id ~key:name ~data:id;
      Some id
  | Some id ->
      let func = Int.Table.find_exn t.functions id in
      if
        func.id = id
        && Raw_addr.equal func.start start
        && Raw_addr.equal func.finish finish
      then Some id
      else if
        (* All ocaml function symbols are global. Local symbols usually come
           from C code. There may be more than one local symbol with the same
           name, all of which will end up in the same function section,
           because function section names are derived from function names by
           default C compilers when -ffunction-sections. We cannot reorder
           one local functions only at link time time. BOLT can do this as
           post link. Some of the C functions we cannot handle because they
           come from the system or libraries, and we do not have control over
           them to recompile them with function sections. *)
        (* CR-someday gyorsh: handle local functions by indexing their names
           correctly. It is not worth doing until we encounter a duplicate
           local function that we actually care about reordering. *)
        func.id = id && i.v.local
        && (!ignore_local_dup || String.is_prefix ~prefix:"_" name)
      then (
        (* CR-soon gyorsh: which function is ignored depends on the order in
           which we encounter the local functions in the samples. If we
           remove the existing function, and encounter the name again, how do
           we know that it is a duplicate? *)
        Report.logf "Ignoring samples in local function symbol %s at 0x%Lx\n"
          name start;
        None )
      else
        Report.user_error
          "Mismatch get_or_add_func_id for %s with start=0x%Lx,finish=0x%Lx\n\
           Found %s in name2id with id=%d but id2func is func.id=%d \
           start=0x%Lx,finish=0x%Lx.\n\
           If it is a local function,\n\
          \           try -ignore-local-dup command line option." name start
          finish name id func.id func.start func.finish ()

let decode_addr t addr interval dbg =
  (* let open Loc in *)
  match interval with
  | None ->
      if !verbose then
        printf "Cannot find function symbol containing 0x%Lx\n" addr;
      false
  | Some interval -> (
      match get_or_add_func_id t interval with
      | None -> false
      | Some id ->
          let start = interval.l in
          let name = interval.v.name in
          let offset =
            match Raw_addr.(to_int (addr - start)) with
            | None ->
                Report.user_error "Offset too big: 0x%Lx"
                  Raw_addr.(addr - start)
                  ()
            | Some offset ->
                assert (offset >= 0);
                offset
          in
          let rel = Some { id; offset } in
          let dbg =
            match dbg with
            | None ->
                if !verbose then
                  Printf.printf "Elf location NOT FOUND at 0x%Lx\n" addr;
                None
            | Some dbg ->
                if !verbose then
                  Printf.printf "%Lx:%Lx:%s:%d\n" addr start dbg.file
                    dbg.line;
                (* Check that the filename has supported suffix and return
                   it. *)
                if Filenames.(compare Linear ~expected:name ~actual:dbg.file)
                then (
                  (* Set has_linearids of this function *)
                  let func = Int.Table.find_exn t.functions id in
                  func.has_linearids <- true;
                  Some dbg.line )
                else (
                  Report.logf "No linear ids in function %s from file %s\n"
                    name dbg.file;
                  None )
          in
          if !verbose then printf "addr2loc adding addr=0x%Lx\n" addr;
          let loc = { rel; dbg } in
          Raw_addr.Table.add_exn t.addr2loc ~key:addr ~data:loc;
          true )

let create locations (agg : Aggregated_perf_profile.t) ~crc_config =
  if !verbose then printf "Decoding perf profile.\n";
  (* Collect all addresses that need decoding. Mispredicts and traces use the
     same addresses as branches, so no need to add them *)
  (* Overapproximation of number of different addresses for creating hashtbl *)
  let size =
    Raw_addr.Table.length agg.instructions
    + (Raw_addr_pair.Table.length agg.branches * 2)
  in
  let addresses = Raw_addr.Table.create ~size () in
  let add key =
    if not (Raw_addr.Table.mem addresses key) then (
      if !verbose then printf "Adding key 0x%Lx\n" key;
      Raw_addr.Table.set addresses ~key ~data:None )
    else if !verbose then printf "Found key 0x%Lx\n" key
  in
  let add2 (fa, ta) =
    add fa;
    add ta
  in
  Raw_addr.Table.iter_keys agg.instructions ~f:add;
  Raw_addr_pair.Table.iter_keys agg.branches ~f:add2;
  (* A key may be used multiple times in keys of t.instruction and t.branches *)
  let len = Raw_addr.Table.length addresses in
  if !verbose then printf "size=%d,len=%d\n" size len;
  assert (len <= size);
  (* Resolve all addresses seen in samples in one pass over the binary. *)
  Elf_locations.resolve_all locations addresses;
  (* CR-someday gyorsh: set config to all true to decode all symbols from the
     binary and store in the profile, even if the user chooses to ignore them
     later. For now, we use crc_config from command line to control what kind
     of crcs to save, with the default being only unit level, because (1) the
     profile can be too big when all function crcs are included and this
     slows down builds that use the profile, (2) reuse is still fragile due
     to ppx and location information. *)
  let crcs = Crcs.(mk Create crc_config) in
  Elf_locations.iter_symbols locations ~func:false ~data:true
    ~f:(fun name _ -> Crcs.decode_and_add crcs name);
  let t = mk len (Crcs.tbl crcs) agg.buildid in
  (* Decode all locations: map addresses to locations. *)
  let resolved =
    Raw_addr.Table.fold addresses ~init:0 ~f:(fun ~key:addr ~data:dbg acc ->
        (* this is cached using interval tree *)
        let interval =
          Elf_locations.resolve_function_containing locations
            ~program_counter:addr
        in
        if decode_addr t addr interval dbg then acc + 1 else acc)
  in
  let total = Raw_addr.Table.length addresses in
  Report.logf "Resolved %d of %d addresses (%.2fl%%)" resolved total
    (Report.percent resolved total);
  create_func_execounts t agg;
  t

let read filename =
  if !verbose then
    printf "Reading aggregated decoded profile from %s\n" filename;
  let t =
    match Parsexp_io.load (module Parsexp.Single) ~filename with
    | Ok t_sexp -> (
        try t_of_sexp t_sexp
        with e ->
          Report.user_error ~hint:(Some Report.Hint.Old_profile)
            ~exn:(Some e)
            "Cannot parse aggregated decoded profile from file %s." filename
        )
    | Error error ->
        Parsexp.Parse_error.report Caml.Format.std_formatter error ~filename;
        Report.user_error "Cannot parse aggregated decoded profile file %s"
          filename
  in
  if !verbose then printf !"Aggregated decoded profile:\n%{sexp:t}\n" t;
  t

let write t filename =
  if !verbose then
    printf "Writing aggregated decoded profile to %s\n" filename;
  let chan = Out_channel.create filename in
  Printf.fprintf chan !"%{sexp:t}\n" t;
  Out_channel.close chan

let write_shape oc =
  let d = Bin_prot.Shape.(eval_to_digest bin_shape_t |> Digest.to_md5) in
  Md5.output_blocking d oc

let read_and_check_shape ic =
  let d = Md5.input_blocking ic in
  let open Bin_prot.Shape in
  if not (0 = Digest.compare (eval_to_digest bin_shape_t) (Digest.of_md5 d))
  then
    Report.user_error ~hint:(Some Report.Hint.Old_profile)
      "Incompatible format of profile file."

let write_bin t filename =
  try
    Out_channel.with_file filename ~f:(fun oc ->
        write_shape oc;
        let buf = Bin_prot.Utils.bin_dump ~header:true bin_writer_t t in
        Bigstring_unix.really_output oc buf)
  with e ->
    Report.user_error ~exn:(Some e) "Failed to write profile to %s" filename

let read_bin filename =
  try
    In_channel.with_file filename ~f:(fun ic ->
        read_and_check_shape ic;
        let read buf ~pos ~len = Bigstring_unix.really_input ic ~pos ~len buf in
        Bin_prot.Utils.bin_read_stream ~read bin_reader_t)
  with e ->
    Report.user_error ~exn:(Some e) "Failed to read profile from %s" filename

let to_sexp filename =
  let t = read_bin filename in
  Printf.printf !"%{sexp:t}" t

let of_sexp ~input_filename ~output_filename =
  let t = read input_filename in
  write_bin t output_filename

(* CR-soon gyorsh: how often do we need to create it? is it worth storing it
   as a field of [t]. It can be computed after the profile is decoded (i.e.,
   need not be on the fly).

   CR-soon gyorsh: for partial profile reuse, we considered adding the new
   function names to name2id mapping, with the id of the old function. This
   can cause id2name become a multiset. Check that profile reuse does not
   confuse all uses of id2name in particular hot function list is generated
   correctly, with multiple and warnings disabled perhaps?

   Alternative is to split function names into a pair of name and index. *)
let id2name t =
  String.Table.to_alist t.name2id
  |> List.Assoc.inverse
  |> Map.of_alist_multi (module Int)
  (* sort for stability of function ordering *)
  |> Map.map ~f:(fun l -> List.sort l ~compare:String.compare)

let all_functions t = String.Table.keys t.name2id

(* Sort functions using preliminary function-level execution counts in
   descending order. *)
let sorted_functions_with_counts t =
  let compare (n1, c1) (n2, c2) =
    (* Descending order of execution counts, i.e., reverse order of int
       compare. *)
    let res = Execount.compare c2 c1 in
    (* Tie breaker using names. Slower than id but more stable w.r.t. changes
       in perf data and ocamlfdo, because ids are an artifact of the way
       ocamlfdo reads and decodes locations. Change to tie breaker using id
       if speed becomes a problem. *)
    if res = 0 then String.compare n1 n2 else res
  in
  String.Table.fold t.name2id ~init:[] ~f:(fun ~key:name ~data:id acc ->
      let func = Int.Table.find_exn t.functions id in
      (name, func.count) :: acc)
  |> List.sort ~compare

let remove_counts l = List.unzip l |> fst

let top_functions t = sorted_functions_with_counts t |> remove_counts

let print_sorted_functions_with_counts t =
  let sorted = sorted_functions_with_counts t in
  Printf.printf "Top functions sorted by execution counters:\n";
  List.iter sorted ~f:(fun (name, count) ->
      Printf.printf !"%Ld %s\n" count name)

let print_stats t =
  (* approximate size using sexp representation of the profile and its
     components *)
  let total = sexp_of_t t |> Sexp.size |> snd in
  let crcs_stats = Crcs.get_stats t.crcs in
  let total_crcs = crcs_stats.func + crcs_stats.unit in
  let funcs = Int.Table.length t.functions in
  let len = Option.value_map ~default:0 ~f:String.length t.buildid in
  let stats =
    [ [ ("names", String.Table.length t.name2id, None);
        ("functions", funcs, None);
        ( "functions with linear ids",
          Int.Table.count t.functions ~f:(fun f -> f.has_linearids),
          Some funcs );
        ("addresses", Raw_addr.Table.length t.addr2loc, None) ];
      [ ("total crcs", total_crcs, Some total_crcs);
        ("func crcs", crcs_stats.func, Some total_crcs);
        ("unit crcs", crcs_stats.unit, Some total_crcs) ];
      [ ("total size of sexp atoms in chars", total, Some total);
        ("buildid", len, Some total);
        ( "addr2loc",
          Raw_addr.Table.sexp_of_t Loc.sexp_of_t t.addr2loc
          |> Sexp.size |> snd,
          Some total );
        ( "functions",
          Int.Table.sexp_of_t Func.sexp_of_t t.functions |> Sexp.size |> snd,
          Some total );
        ("crcs", Crcs.sexp_of_tbl t.crcs |> Sexp.size |> snd, Some total) ];
      [] ]
  in
  Printf.printf "Profile size and stats:\n";
  let pp part = function
    | None -> "        "
    | Some whole -> sprintf "(%5.1f%%)" (Report.percent part whole)
  in
  List.iter stats ~f:(fun sizes ->
      Printf.printf
        "-----------------------------------------------------------------\n";
      List.iter sizes ~f:(fun (title, size, total_size) ->
          Printf.printf "%15d %s %s\n" size (pp size total_size) title));
  ()

(** Trim the profile in place, by keeping information only about functions
    for which [keep] returns true. *)
let trim t ~keep =
  String.Table.filteri_inplace t.name2id ~f:(fun ~key:name ~data:id ->
      let res = keep name in
      if not res then (
        Int.Table.remove t.functions id;
        Raw_addr.Table.filter_inplace t.addr2loc ~f:(fun loc ->
            match loc.rel with
            | None -> true
            | Some rel -> not (rel.id = id)) );
      res)

let trim_functions t ~cutoff =
  match cutoff with
  | [] -> ()
  | _ ->
      let top =
        sorted_functions_with_counts t
        |> Trim.apply cutoff |> remove_counts |> String.Set.of_list
      in
      trim t ~keep:(String.Set.mem top)

let rename t old2new =
  String.Table.map_inplace t.name2id ~f:(fun id ->
      Int.Table.find_exn old2new id);
  Raw_addr.Table.map_inplace t.addr2loc ~f:(Loc.rename ~old2new);
  let size = Int.Table.length t.functions in
  let functions = Int.Table.create ~size () in
  Int.Table.iteri old2new ~f:(fun ~key:id ~data:newid ->
      let func = Int.Table.find_exn t.functions id in
      Int.Table.set functions ~key:newid ~data:{ func with id = newid });
  assert (Int.Table.length functions = size);
  { t with functions }

type patch =
  { from : int;
    newid : int
  }
[@@deriving sexp]

(* CR-soon gyorsh: use information from t.crcs when merging functions.
   WARNING: modifies the input profiles inplace in memory, to avoid
   allocations. Does not modify the files.

   If crc checks are disabled (-on-md5-mismatch command line option), then a
   mismatch in crcs is reported in verbose and then we can do one of the
   following alternatives:

   1) ignore the mismatch: functions are considered identical for the purpose
   of merging their execution counts. 2) keep both copies of the function,
   and use the one that applies when compiling with fdo. 3) remove both
   functions from the profile when there is a mismatch because we do not know
   which is the current/more recent. 4) keep only the most recent, as
   specified by the user (need extra argument).

   Current implementation is effectively (1), because CRC are not used when
   merging the functions. CR-soon gyorsh: enable the above alternatives
   through a command line option, for experiments.

   When ignoring buildid and crcs, it is possible to merge profiles from
   binaries that differ only in addresses, for example, profiles collected
   from different layouts of the same code, as long as two versions of the
   function they don't overlap. That is, no basic block reordering. hmm. *)
let merge_into ~src ~dst ~crc_config ~ignore_buildid =
  dst.buildid <- Merge.buildid src.buildid dst.buildid ~ignore_buildid;
  (* refresh ids of function in src, so as not to clash with dst: if func is
     in both src and dst, use the id from dst, otherwise rename src id to a
     fresh id. *)
  let last = ref (String.Table.length dst.name2id) in
  let fresh () =
    let res = !last in
    incr last;
    res
  in
  let old2new =
    Int.Table.create ~size:(String.Table.length src.name2id) ()
  in
  let patches = ref [] in
  let merge_name2id ~key a b : (_ Hashtbl.Merge_into_action.t) =
    let id = Int.Table.find old2new a in
    if Option.is_some id && !verbose then
      (* Two different names mapped to the same id in src. currently cannot
         happen in profiles saved to file. Handle it here anyway, to ensure
         all possible agg_dec_profiles are handled correctly. *)
      Printf.printf
        "Duplicate use of id. Different function names\n\
        \               mapped to the same id by profile. One of them is \
         %s, original id is %d, mapped to new id %d.\n"
        key a (Option.value_exn id);
    let newid =
      match (id, b) with
      | None, None -> fresh ()
      | Some id, None -> id
      | None, Some b -> b
      | Some id, Some b ->
          if not (b = id) then (
            if !verbose then
              Printf.printf "patching old2new for old id %d from %d to %d" a
                id b;
            patches := { from = id; newid = b } :: !patches );
          b
    in
    Int.Table.set old2new ~key:a ~data:newid;
    Set_to newid
  in
  if !verbose then (
    Printf.printf !"src:\n%{sexp:int String.Table.t}\n" src.name2id;
    Printf.printf !"dst:\n%{sexp:int String.Table.t}\n" dst.name2id );
  String.Table.merge_into ~src:src.name2id ~dst:dst.name2id ~f:merge_name2id;
  ( if not (List.is_empty !patches) then
      (* patch dst.name2id: needed if two names in src mapped to the same id. *)
      (* CR-soon gyorsh: this is completely untested. *)
      if !verbose then (
        Printf.printf !"patch:\n%{sexp:patch List.t}\n" !patches;
        Printf.printf
          !"dst before patch:\n%{sexp:int String.Table.t}\n"
          dst.name2id );
    let id2name = id2name dst in
    List.iter !patches ~f:(fun { from; newid } ->
        let names = Map.find_exn id2name from in
        assert (List.length names > 0);
        List.iter names ~f:(fun name ->
            String.Table.set dst.name2id ~key:name ~data:newid)) );
  if !verbose then
    Printf.printf !"old2new:\n%{sexp:int Int.Table.t}\n" old2new;
  let src = rename src old2new in
  if !verbose then (
    Printf.printf !"src:\n%{sexp:int String.Table.t}\n" src.name2id;
    Printf.printf !"dst:\n%{sexp:int String.Table.t}\n" dst.name2id );
  let merge_addr2loc ~key:_ a b : (_ Hashtbl.Merge_into_action.t) =
    match b with
    | None -> Set_to a
    | Some b -> Set_to (Loc.merge a b)
  in
  let merge_functions ~key:_ a b : (_ Hashtbl.Merge_into_action.t) =
    match b with
    | None -> Set_to a
    | Some b -> Set_to (Func.merge a b ~crc_config ~ignore_buildid)
  in
  Raw_addr.Table.merge_into ~src:src.addr2loc ~dst:dst.addr2loc
    ~f:merge_addr2loc;
  Int.Table.merge_into ~src:src.functions ~dst:dst.functions
    ~f:merge_functions;
  Crcs.merge_into ~src:src.crcs ~dst:dst.crcs crc_config

module Merge = Merge.Make (struct
  type nonrec t = t

  let merge_into = merge_into

  let read = read_bin

  let write = write_bin

  let approx_size t =
    Raw_addr.Table.length t.addr2loc + String.Table.length t.name2id
end)
