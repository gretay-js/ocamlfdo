(* Perf profile format as output of perf script -F pid,ip,brstack *)

open Core

let verbose = ref true

type mispredict_flag =
  | M
  | P
  | NOT_SUPPORTED
[@@deriving compare, sexp]

let mispredicted = function
  | M -> true
  | _ -> false

type br = {
  from_addr : Addr.t;
  to_addr : Addr.t;
  mispredict : mispredict_flag;
  index : int;
      (* Position on the stack, with 0 being the most recent branch. This
         field is only used for only for validation. *)
      (* cycles : int; *)
}
[@@deriving compare, sexp]

type sample = {
  ip : Addr.t;
  (* instruction pointer where the sample was taken *)
  brstack : br list; (* branch stack is the last branch record (LBR) *)
}
[@@deriving compare, sexp]

type t = sample list [@@deriving compare, sexp]

let parse_br index s =
  match String.split s ~on:'/' with
  | [ from_addr; to_addr; m; t; a; c ] ->
      let mispredict =
        match m with
        | "M" -> M
        | "P" -> P
        | "-" -> NOT_SUPPORTED
        | _ ->
            Report.user_error "Cannot parse mispredict flag %s in %s" m s ()
      in
      (* Parse and ignore t and a flags. *)
      ( match t with
      | "X" | "-" -> ()
      | _ ->
          Report.user_error "Cannot parse mispredict flag %s in %s" m s ()
      );
      ( match a with
      | "A" | "-" -> ()
      | _ ->
          Report.user_error "Cannot parse mispredict flag %s in %s" a s ()
      );

      (* Parse and ignore cycles. CR-soon gyorsh: use for optimizations. *)
      let _cycles = Int.of_string c in
      {
        from_addr = Int64.of_string from_addr;
        to_addr = Int64.of_string to_addr;
        index;
        mispredict;
      }
  | _ -> Report.user_error "Cannot parse %s\n" s ()

(* The most recent branch is printed first by perf script. The number of
   branch entries vary based on the underlying hardware. This function
   reverses the stack from its perf profile order. *)
let rec parse_brstack (index, brstack) row =
  match row with
  | [] -> (index, brstack)
  | hd :: tl ->
      let brstack = parse_br index hd :: brstack in
      parse_brstack (index + 1, brstack) tl

let split_on_whitespace row =
  let r = String.split ~on:' ' row in
  List.filter r ~f:(fun s -> not (String.is_empty s))

let hex s = if String.is_prefix ~prefix:"0x" s then s else "0x" ^ s

type mmap = {
  pid : int;
  name : string;
  base : Addr.t;
  size : Addr.t;
}

type row =
  | Unknown
  | Sample of sample
  | Mmap of mmap

(* CR-soon gyorsh: this is just parsing a regexp like a monkey, and it is
   getting ugly and probably slow. use regexp parser? *)
let row_to_sample ~keep_pid row =
  match split_on_whitespace row with
  | pid :: "PERF_RECORD_MMAP2" :: id :: pos :: rest ->
      if !verbose then
        printf "parsing PERF_RECORD_MMAP2 for pid=%s:\n%s\n" pid row;
      let pid = Int.of_string pid in
      if keep_pid pid then
        match List.last rest with
        | None -> Report.user_error "Unexpected format"
        | Some name ->
            if String.is_prefix ~prefix:"/" name then (
              (* This is just a sanity check for the format *)
              let id =
                String.chop_suffix id ~suffix:":" |> Option.value_exn
              in
              ( match String.split id ~on:'/' with
              | [ n; _tid ] ->
                  (* Do we need to do anything with tid? what does it mean? *)
                  assert (Int.of_string n = pid)
              | _ -> Report.user_error "Unexpected format" );

              (* parse the important info *)
              match String.split_on_chars pos ~on:[ '['; '('; ')' ] with
              | [ base; size ] ->
                  let base = Addr.of_string base in
                  let size = Addr.of_string size in
                  Mmap { pid; name; base; size }
              | _ -> Report.user_error "Unexpected format" )
            else Unknown
      else Unknown
  | pid :: ip :: rest ->
      let pid = Int.of_string pid in
      if keep_pid pid then (
        if !verbose then printf "parsing ip %s\n" ip;
        let sample =
          {
            ip = Int64.of_string (hex ip);
            brstack = snd (parse_brstack (0, []) rest);
          }
        in
        if !verbose then (
          printf "raw brstack=%s\n" row;
          printf "parsed brstack=";
          List.iter sample.brstack ~f:(fun br ->
              printf "0x%Lx/0x%Lx " br.from_addr br.to_addr);
          printf "\n" );
        Sample sample )
      else Unknown
  | _ -> Report.user_error "Cannot parse %s\n" row ()

let pids = ref Int.Set.empty

let check_keep_pid expected_pids p ~ignore_buildid =
  if !verbose then
    if not (Int.Set.mem !pids p) then (
      printf "Found new pid: %d\n" p;
      pids := Int.Set.add !pids p );
  if ignore_buildid then true
  else
    let keep = Int.Set.mem expected_pids p in
    if (not keep) && !verbose then
      printf
        !"Ignoring pid %d, not in expected %{sexp:Int.Set.t}).\n"
        p expected_pids;
    keep

let script = [ "script"; "-F"; "pid,ip,brstack" ]

let buildid = [ "buildid-list"; "-f" ]

let perf_fold filename args ~init ~f =
  let args = List.concat [ [ "perf" ]; args; [ "-i"; filename ] ] in
  let open Shexp_process in
  let open Shexp_process.Infix in
  let f x y = return (f x y) in
  let t = eval (err_to_out (call args) |- fold_lines ~init ~f) in
  t

let read filename =
  if !verbose then
    printf
      "Reading perf profile generated by \"perf script -F pid,ip,brstack\" \
       from %s\n"
      filename;
  let keep_pid = check_keep_pid Int.Set.empty ~ignore_buildid:true in
  let f acc row =
    match row_to_sample ~keep_pid (String.strip row) with
    | Unknown -> acc
    | Sample sample -> sample :: acc
    | Mmap _ ->
        Report.user_error "Unexpected mmap event in perf script output"
  in
  let t = perf_fold filename script ~init:[] ~f in
  if !verbose then (
    Printf.printf !"%{sexp:t}\n" t;
    Printf.printf "Found pids:\n";
    Int.Set.iter !pids ~f:(fun pid -> Printf.printf "%d\n" pid) );
  t

type stats = {
  ignored : int;
  total : int;
  lbr : int;
}

let inc table key =
  Hashtbl.update table key ~f:(fun v ->
      Int64.(1L + Option.value ~default:0L v))

let aggregate_br prev cur is_last (aggregated : Aggregated_perf_profile.t) =
  (* Instructions executed between branches can be inferred from brstack *)
  let add_br () =
    let key = (cur.from_addr, cur.to_addr) in
    inc aggregated.branches key;
    if mispredicted cur.mispredict then inc aggregated.mispredicts key
  in
  match prev with
  | None -> add_br ()
  | Some prev ->
      assert (prev.index = cur.index + 1);
      let from_addr = prev.to_addr in
      let to_addr = cur.from_addr in
      if !verbose then printf "cur 0x%Lx->0x%Lx\n" cur.from_addr cur.to_addr;
      if !verbose then printf "trace 0x%Lx->0x%Lx\n" from_addr to_addr;

      (* There appear to be a problem with perf output: last LBR entry is
         repeated twice sometimes. It may be related to the recent problem
         mentioned in a patch for perf script: Fix LBR skid dump problems in
         brstackinsn https://github.com/torvalds/linux/commit
         /61f611593f2c90547cb09c0bf6977414454a27e6 *)
      let dup =
        prev.from_addr = cur.from_addr && prev.to_addr = cur.to_addr
      in
      let mis_prev = mispredicted prev.mispredict in
      let mis_cur = mispredicted cur.mispredict in
      let fallthrough_backwards = from_addr >= to_addr in
      if dup then
        if !verbose then
          printf
            "Duplicate entry in LBR: 0x%Lx->0x%Lx mis_prev=%b mis_cur=%b \
             last=%b (from_addr >= to_addr)=%b\n"
            prev.from_addr prev.to_addr mis_prev mis_cur is_last
            fallthrough_backwards;

      if dup && is_last then (
        if !verbose then
          printf "Duplicated last LBR entry is ignored: 0x%Lx->0x%Lx\n"
            from_addr to_addr;
        if not fallthrough_backwards then
          if !verbose then
            printf
              "Duplicate last entry without fallthrough backwards is \
               unexpected 0x%Lx->0x%Lx.\n"
              from_addr to_addr )
      else (
        (* branches *)
        add_br ();

        (* fallthrough traces *)
        if fallthrough_backwards then (
          if !verbose then
            printf
              "Malformed trace 0x%Lx->0x%Lx (from_addr >= to_addr), it was \
               not duplicated last entry.\n"
              from_addr to_addr )
        else
          let key = (from_addr, to_addr) in
          inc aggregated.traces key )

(* CR-soon gyorsh: aggregate during parsing of perf profile *)
let rec aggregate_brstack prev brstack aggregated =
  match brstack with
  | [] -> ()
  | cur :: tl ->
      let is_last = List.is_empty tl in
      aggregate_br prev cur is_last aggregated;
      aggregate_brstack (Some cur) tl aggregated

let aggregate sample (aggregated : Aggregated_perf_profile.t) =
  inc aggregated.instructions sample.ip;
  aggregate_brstack None sample.brstack aggregated

let extract_pids data perf_data =
  (* find pids *)
  (* there seems to be no better way of mapping dso to pids, or restricting
     the output of `perf script` to a particular dso. With `perf script`,
     dso appearing attached to every address in brstacks, which makes the
     output much bigger and longer to process. *)
  (* CR-soon replace this hack with parsing the output of perf report, or a
     better way to extract pids. *)
  (* We could also extract buildid and dso association from this output,
     without calling buildid-list before it, but while the call to
     buildid-list seems reasonable to keep long term, this function should
     be replaced. *)
  perf_fold perf_data [ "report"; "-F"; "dso,pid"; "--stdio"; "-v" ]
    ~init:Int.Set.empty ~f:(fun acc s ->
      Printf.printf "[find pids] %s\n" s;
      if
        String.equal s ""
        || String.is_prefix s ~prefix:"build id event received for "
        || String.is_prefix s ~prefix:"#"
      then acc
      else
        try
          let s = String.strip s in
          let dso = String.prefix s (String.index_exn s ' ') in
          if !verbose then Printf.printf "[find pids] dso=%s\n" dso;
          match List.find data ~f:(String.equal dso) with
          | None -> acc
          | Some _ -> (
              let pid_comm =
                String.drop_prefix s (String.rindex_exn s ' ' + 1)
              in
              if !verbose then
                Printf.printf "[find pids] pid_comm=%s\n" pid_comm;
              match String.split ~on:':' pid_comm with
              | [ pid; _ ] ->
                  if !verbose then Printf.printf "[find pids] pid=%s\n" pid;
                  Int.Set.add acc (Int.of_string pid)
              | _ ->
                  Report.user_error
                    "Cannot find pid. Unexpect output of perf report:%s\n" s
              )
        with _ ->
          Report.user_error
            "Cannot find pid. Unexpect output of perf report:%s\n" s)

let check_buildid binary perf_data ignore_buildid =
  let binary_buildid =
    perf_fold binary buildid ~init:[] ~f:(fun acc s -> s :: acc)
    |> List.hd_exn
  in
  let parse row =
    match String.split ~on:' ' row with
    | [ buildid; dso ] ->
        if String.equal binary_buildid buildid then Some dso else None
    | _ ->
        Report.user_error "Unexpected output format of `perf %s -i %s`:\n%s"
          (String.concat ~sep:" " buildid)
          perf_data row
  in
  let data =
    perf_fold perf_data buildid ~init:[] ~f:(fun acc s ->
        match parse s with
        | None -> acc
        | Some d -> d :: acc)
  in
  ( match data with
  | [ dso ] ->
      if !verbose then
        printf "Found comm for buildid %s: %s\n" binary_buildid dso
  | [] ->
      let msg () =
        sprintf
          "buildid mismatch: buildid %s of binary %s is not found in perf \
           profile %s."
          binary_buildid binary perf_data
      in
      if ignore_buildid then (
        if !verbose then printf "%s\nbuildid mismatch ignored\n" (msg ()) )
      else
        Report.user_error
          "%s\n\
           To check, compare %s to output of `perf %s -i %s`\n\
           To ignore, add -ignore-buildid option when calling `ocamlfdo \
           decode`."
          (msg ()) binary_buildid
          (String.concat ~sep:"" buildid)
          perf_data
  | _ -> () );
  if !verbose then (
    printf "Found %d comms for buildid %s in %s." (List.length data)
      binary_buildid perf_data;
    if !verbose then List.iter data ~f:(printf "%s\n") );
  extract_pids data perf_data

let pids_to_keep ~found_pids ~expected_pids =
  match expected_pids with
  | [] ->
      if !verbose then
        printf "ignoring empty expected_pids, using found pids\n";
      found_pids
  | _ ->
      let expected_pids = Int.Set.of_list expected_pids in
      let is_subset = Int.Set.is_subset expected_pids ~of_:found_pids in
      if !verbose then (
        printf
          "expected pids is %s subset of pids found in profile that match \
           the buildid of the executable.\n"
          (if is_subset then "a" else "not a");
        printf !"user's expected pids = %{sexp:Int.Set.t}\n" expected_pids;
        printf !"found pids = %{sexp:Int.Set.t}\n" found_pids );
      if is_subset then expected_pids else found_pids

let read_and_aggregate filename binary ignore_buildid expected_pids =
  if !verbose then printf "Aggregate perf profile from %s.\n" filename;

  (* check that buildid of the binary matches buildid of some dso sampled in
     the profile, and return the pids with which that dso appears in the
     profile. *)
  let found_pids =
    if (not ignore_buildid) || !verbose then
      check_buildid binary filename ignore_buildid
    else Int.Set.empty
  in
  let keep_pid =
    check_keep_pid ~ignore_buildid (pids_to_keep ~found_pids ~expected_pids)
  in
  let aggregated = Aggregated_perf_profile.empty () in
  let empty_stats = { ignored = 0; total = 0; lbr = 0 } in
  let f stats row =
    match row_to_sample ~keep_pid row with
    | Unknown ->
        { stats with ignored = stats.ignored + 1; total = stats.total + 1 }
    | Sample sample ->
        aggregate sample aggregated;
        {
          stats with
          total = stats.total + 1;
          lbr = stats.lbr + List.length sample.brstack;
        }
    | Mmap _mmap -> stats
    (* CR-soon gyorsh: use memory map to translate ip addresses of samples
       (ip=instruction pointer is a virtual memory address) to offsets into
       the binary. If mmap is not available, warn the user and continue,
       assuming direct mapping.

       Without it, the dwarf info might be wrong or not found when the
       profile is later decoded, if ASLR is enabled, relocations happened or
       dynamically linked code was sampled.

       Why wouldn't mmap be available in perf data? It may be disabled when
       calling 'perf record' to make the processing faster. What if perf
       attached to a process that is already running - will there still be a
       mmap event recorded? If not, the user should be able to save the mmap
       information and supply it in some format to ocamlfdo decode.

       If mmapping might change in the middle of execution (does ASLR do it?
       other machinery?), will there be an event recorded for each change?
       If so, do the samples need to be interpreted w.r.t. the appropriate
       event preceeding mmap event, so we need to either keep track of the
       order they are listed (is the output of perf script guaranteed to be
       ordered by the time of samples) or parse the time of the samples and
       map ip addresses accroding to their order relative to map events. *)
  in
  let stats = perf_fold filename script ~init:empty_stats ~f in
  if !verbose then (
    Printf.printf "Read %d samples with %d LBR entries\n" stats.total
      stats.lbr;
    let r = Float.(of_int stats.ignored * 100.0 / of_int stats.total) in
    Printf.printf "Ignored %d samples (%.1f)\n" stats.ignored r;
    Printf.printf "Found pids:\n";
    Int.Set.iter !pids ~f:(fun pid -> printf "%d\n" pid) );
  aggregated
