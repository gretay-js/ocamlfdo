open Core
open Dbg
open Loc
open Func
module Cfg_with_layout = Ocamlcfg.Cfg_with_layout
module Cfg = Ocamlcfg.Cfg

let verbose = ref true

let magic_number = "ocamlfdo001"

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
  match Hashtbl.find t.addr2loc addr with
  | None ->
      printf "Not found any cached location for address 0x%Lx\n" addr;
      assert false
  | Some loc -> (
      match loc.rel with
      | None -> None
      | Some rel ->
          let id = rel.id in
          let func = Hashtbl.find_exn t.functions id in
          Some func )

(* Source or target addr of this branch does not belong to the binary and
   usually to_addr cannot be target of jump because it is not a return from a
   call instruction and not an entry function. Could it be a context switch? *)
let track_branch (from_addr, to_addr) =
  if Int64.(from_addr < 0L) || Int64.(to_addr < 0L) then (
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
  Hashtbl.iteri agg.instructions ~f:(fun ~key ~data ->
      match get_func t key with
      | None -> ()
      | Some func ->
          func.count <- Int64.(func.count + data);
          Hashtbl.add_exn func.agg.instructions ~key ~data);
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
  Hashtbl.iteri agg.branches ~f:(fun ~key ~data ->
      let mispredicts =
        Option.value (Hashtbl.find agg.mispredicts key) ~default:0L
      in
      let update_br func =
        func.count <- Int64.(func.count + data);
        if track_branch key then (
          Hashtbl.add_exn func.agg.branches ~key ~data;
          if Int64.(mispredicts > 0L) then
            Hashtbl.add_exn func.agg.mispredicts ~key ~data:mispredicts )
      in
      process key update_br);
  Hashtbl.iteri agg.traces ~f:(fun ~key ~data ->
      (* traces don't contribute to func's total count because it is
         accounted for in branches. *)
      let update_tr func =
        if track_branch key then Hashtbl.add_exn func.agg.traces ~key ~data
      in
      process key update_tr)

(* Find or add the function and return its id *)
let get_func_id t ~name ~start ~finish =
  match Hashtbl.find t.name2id name with
  | None ->
      let id = Hashtbl.length t.functions in
      let func = Func.mk ~id ~start ~finish in
      Hashtbl.add_exn t.functions ~key:id ~data:func;
      Hashtbl.add_exn t.name2id ~key:name ~data:id;
      func.id
  | Some id ->
      let func = Hashtbl.find_exn t.functions id in
      assert (func.id = id);
      assert (Raw_addr.equal func.start start);
      assert (Raw_addr.equal func.finish finish);
      func.id

let decode_addr t addr interval dbg =
  let open Loc in
  let loc =
    match interval with
    | None ->
        if !verbose then
          printf "Cannot find function symbol containing 0x%Lx\n" addr;
        { rel = None; dbg = None }
    | Some interval ->
        let open Intervals in
        let name = interval.v in
        let start = interval.l in
        let finish = interval.r in
        let offset =
          match Int64.(to_int (addr - start)) with
          | None ->
              Report.user_error "Offset too big: 0x%Lx"
                Raw_addr.(addr - start)
                ()
          | Some offset ->
              assert (offset >= 0);
              offset
        in
        let id = get_func_id t ~name ~start ~finish in
        let rel = Some { id; offset; label = None } in
        let dbg =
          match dbg with
          | None ->
              if !verbose then
                Printf.printf "Elf location NOT FOUND at 0x%Lx\n" addr;
              None
          | Some dbg ->
              if !verbose then
                Printf.printf "%Lx:%Lx:%s:%d\n" addr start dbg.file dbg.line;

              (* Check that the filename has supported suffix and return it. *)
              if Filenames.(compare Linear ~expected:name ~actual:dbg.file)
              then (
                (* Set has_linearids of this function *)
                let func = Hashtbl.find_exn t.functions id in
                func.has_linearids <- true;
                Some dbg.line )
              else None
        in
        if !verbose then printf "addr2loc adding addr=0x%Lx\n" addr;
        { rel; dbg }
  in
  if Option.is_some loc.rel then
    Hashtbl.add_exn t.addr2loc ~key:addr ~data:loc

let create locations (agg : Aggregated_perf_profile.t) =
  if !verbose then printf "Decoding perf profile.\n";
  (* Collect all addresses that need decoding. Mispredicts and traces use the
     same addresses as branches, so no need to add them *)
  (* Overapproximation of number of different addresses for creating hashtbl *)
  let size =
    Hashtbl.length agg.instructions + (Hashtbl.length agg.branches * 2)
  in
  let addresses = Raw_addr.Table.create ~size () in
  let add key =
    if not (Hashtbl.mem addresses key) then (
      if !verbose then printf "Adding key 0x%Lx\n" key;
      Hashtbl.set addresses ~key ~data:None )
    else if !verbose then printf "Found key 0x%Lx\n" key
  in
  let add2 (fa, ta) =
    add fa;
    add ta
  in
  Hashtbl.iter_keys agg.instructions ~f:add;
  Hashtbl.iter_keys agg.branches ~f:add2;

  (* A key may be used multiple times in keys of t.instruction and t.branches *)
  let len = Hashtbl.length addresses in
  if !verbose then printf "size=%d,len=%d\n" size len;
  assert (len <= size);
  (* Resolve all addresses seen in samples in one pass over the binary. *)
  Elf_locations.resolve_all locations addresses;
  (* set config to all true to decode all symbols from the binary and store
     in the profile, even if the user chooses to ignore them later. *)
  let crc_config = { Crcs.unit = true; func = false } in
  let crcs = Crcs.(mk Create crc_config) in
  Elf_locations.iter_symbols locations ~f:(Crcs.decode_and_add crcs);
  let t = mk len (Crcs.tbl crcs) agg.buildid in
  (* Decode all locations: map addresses to locations. *)
  Hashtbl.iteri addresses ~f:(fun ~key:addr ~data:dbg ->
      (* this is cached using interval tree *)
      let interval =
        Elf_locations.resolve_function_containing locations
          ~program_counter:addr
      in
      decode_addr t addr interval dbg);
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
        Bigstring.really_output oc buf)
  with e ->
    Report.user_error ~exn:(Some e) "Failed to write profile to %s" filename

let read_bin filename =
  try
    In_channel.with_file filename ~f:(fun ic ->
        read_and_check_shape ic;
        let read buf ~pos ~len = Bigstring.really_input ic ~pos ~len buf in
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
   need not be on the fly). *)
let id2name t =
  Hashtbl.to_alist t.name2id
  |> List.Assoc.inverse
  |> Map.of_alist_exn (module Int)

(* Sort functions using preliminary function-level execution counts in
   descending order. *)
let top_functions t =
  let id2name = id2name t in
  let name func = Map.find_exn id2name func.id in
  let compare f1 f2 =
    (* Descending order of execution counts, i.e., reverse order of int
       compare. *)
    let res = Int64.compare f2.count f1.count in
    (* Tie breaker using names. Slower than id but more stable w.r.t. changes
       in perf data and ocamlfdo, because ids are an artifact of the way
       ocamlfdo reads and decodes locations. Change to tie breaker using id
       if speed becomes a problem. *)
    if res = 0 then String.compare (name f1) (name f2) else res
  in
  let sorted = List.sort (Hashtbl.data t.functions) ~compare in
  (* reverse the mapping: from name2id to id2name *)
  let fl = List.map sorted ~f:name in
  fl

let rename t old2new =
  Hashtbl.map_inplace t.name2id ~f:(fun id -> Hashtbl.find_exn old2new id);
  Hashtbl.map_inplace t.addr2loc ~f:(Loc.rename ~old2new);
  let size = Hashtbl.length t.functions in
  let functions = Int.Table.create ~size () in
  Hashtbl.iteri old2new ~f:(fun ~key:id ~data:newid ->
      let func = Hashtbl.find_exn t.functions id in
      Hashtbl.set functions ~key:newid ~data:{ func with id = newid });
  assert (Hashtbl.length functions = size);
  { t with functions }

(* CR-soon gyorsh: use information from t.crcs when merging functions.
   WARNING: modifies the input profiles inplace, to avoid allocations.

   If crc checks are disabled (-no-md5 command line option), then a mismatch
   in crcs is reported in verbose and then we can do one of the following
   alternatives:

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
   from different layouts of the same code. *)
let merge_into ~src ~dst ~crc_config ~ignore_buildid =
  dst.buildid <- Merge.buildid src.buildid dst.buildid ~ignore_buildid;
  (* refresh ids of function in src, so as not to clash with dst: if func is
     in both src and dst, use the id from dst, otherwise rename src id to a
     fresh id. *)
  let fresh = ref (Hashtbl.length dst.name2id) in
  let old2new = Int.Table.create ~size:(Hashtbl.length src.name2id) () in
  let merge_name2id ~key:_ a b =
    let newid =
      match b with
      | None ->
          let newid = !fresh in
          fresh := !fresh + 1;
          newid
      | Some b -> b
    in
    Hashtbl.add_exn old2new ~key:a ~data:newid;
    Hashtbl.Set_to newid
  in
  if !verbose then (
    Printf.printf !"src:\n%{sexp:(string, int) Hashtbl.t}\n" src.name2id;
    Printf.printf !"dst:\n%{sexp:(string, int) Hashtbl.t}\n" dst.name2id );
  Hashtbl.merge_into ~src:src.name2id ~dst:dst.name2id ~f:merge_name2id;
  if !verbose then
    Printf.printf !"old2new:\n%{sexp:(int, int) Hashtbl.t}\n" old2new;
  let src = rename src old2new in
  if !verbose then (
    Printf.printf !"src:\n%{sexp:(string, int)Hashtbl.t}\n" src.name2id;
    Printf.printf !"dst:\n%{sexp:(string, int) Hashtbl.t}\n" dst.name2id );
  let merge_addr2loc ~key:_ a = function
    | None -> Hashtbl.Set_to a
    | Some b -> Hashtbl.Set_to (Loc.merge a b)
  in
  let merge_functions ~key:_ a = function
    | None -> Hashtbl.Set_to a
    | Some b -> Hashtbl.Set_to (Func.merge a b ~crc_config ~ignore_buildid)
  in
  Hashtbl.merge_into ~src:src.addr2loc ~dst:dst.addr2loc ~f:merge_addr2loc;
  Hashtbl.merge_into ~src:src.functions ~dst:dst.functions ~f:merge_functions;
  Crcs.merge_into ~src:src.crcs ~dst:dst.crcs crc_config

module Merge = Merge.Make (struct
  type nonrec t = t

  let merge_into = merge_into

  let read = read_bin

  let write = write_bin

  let approx_size t = Hashtbl.length t.addr2loc + Hashtbl.length t.name2id
end)
