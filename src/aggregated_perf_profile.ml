open Core

let verbose = ref false

type t =
  { instructions : Execount.t Raw_addr.Table.t;
    branches : Execount.t Raw_addr_pair.Table.t;
    mispredicts : Execount.t Raw_addr_pair.Table.t;
    traces : Execount.t Raw_addr_pair.Table.t;
    mutable buildid : string option
  }
[@@deriving sexp, bin_io]

let empty () =
  { instructions = Raw_addr.Table.create ();
    branches = Raw_addr_pair.Table.create ();
    mispredicts = Raw_addr_pair.Table.create ();
    traces = Raw_addr_pair.Table.create ();
    buildid = None
  }

let read filename =
  if !verbose then
    printf "Reading aggregated perf profile from %s\n" filename;
  let t =
    match Parsexp_io.load (module Parsexp.Single) ~filename with
    | Ok t_sexp -> t_of_sexp t_sexp
    | Error error ->
        Parsexp.Parse_error.report Caml.Format.std_formatter error ~filename;
        Report.user_error "Cannot parse aggregated profile file %s" filename
  in
  if !verbose then Printf.printf !"Aggregated perf profile:\n%{sexp:t}\n" t;
  t

let write t filename =
  if !verbose then printf "Writing aggregated perf profile to %s\n" filename;
  let chan = Out_channel.create filename in
  Printf.fprintf chan !"%{sexp:t}\n" t;
  Out_channel.close chan

let merge_into ~src ~dst ~ignore_buildid =
  dst.buildid <- Merge.buildid src.buildid dst.buildid ~ignore_buildid;
  let merge_execounts ~key:_ a b : (_ Hashtbl.Merge_into_action.t) =
    match b with
    | None -> Set_to a
    | Some b -> Set_to Execount.(a + b)
  in
  Hashtbl.merge_into ~src:src.instructions ~dst:dst.instructions
    ~f:merge_execounts;
  Hashtbl.merge_into ~src:src.branches ~dst:dst.branches ~f:merge_execounts;
  Hashtbl.merge_into ~src:src.mispredicts ~dst:dst.mispredicts
    ~f:merge_execounts;
  Hashtbl.merge_into ~src:src.traces ~dst:dst.traces ~f:merge_execounts

module Merge = Merge.Make (struct
  type nonrec t = t

  let read = read

  let write = write

  let approx_size t =
    Hashtbl.length t.instructions
    + Hashtbl.length t.branches + Hashtbl.length t.traces

  let merge_into ~src ~dst ~crc_config:_ ~ignore_buildid =
    merge_into ~src ~dst ~ignore_buildid
end)
