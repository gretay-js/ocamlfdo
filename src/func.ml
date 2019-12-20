open Core

type t =
  { (* Unique identifier we assign to this function *)
    id : int;
    (* Raw start address of the function in original binary *)
    start : Raw_addr.t;
    finish : Raw_addr.t;
    (* Preliminary execution count *)
    mutable count : Execount.t;
    (* Does the function have any linearids? *)
    mutable has_linearids : bool;
    (* number of fallthrough traces that didn't correspond to the cfg *)
    mutable malformed_traces : Execount.t;
    (* Counters that refer to this function, uses raw addresses. *)
    (* CR-soon gyorsh: This can be dropped after cfg_count is constructed, to
       save memory. *)
    agg : Aggregated_perf_profile.t
  }
[@@deriving sexp]

let mk ~id ~start ~finish =
  { id;
    start;
    finish;
    has_linearids = false;
    count = 0L;
    malformed_traces = 0L;
    agg = Aggregated_perf_profile.empty ()
  }

(* Descending order of execution counts (reverse order of compare).Tie
   breaker using id. Slower than id but more stable w.r.t. changes in perf
   data and ocamlfdo, because ids are an artifact of the way ocamlfdo reads
   and decodes locations. Change to tie breaker using id if speed becomes a
   problem. *)
let compare f1 f2 =
  let res = Int64.compare f2.count f1.count in
  if res = 0 then Int.compare f1.id f2.id else res

let merge t1 t2 ~crc_config ~ignore_buildid =
  if
    not
      ( t1.id = t2.id
      && ( Raw_addr.equal t1.start t2.start
           && Raw_addr.equal t1.finish t2.finish
         || ignore_buildid ) )
  then
    Report.user_error
      !"Cannot merge functions:\n%{sexp:t}\n%{sexp:t}\n"
      t1 t2;

  { id = t1.id;
    start = t1.start;
    finish = t1.finish;
    has_linearids = t1.has_linearids || t2.has_linearids;
    count = Execount.(t1.count + t2.count);
    malformed_traces = Execount.(t1.malformed_traces + t2.malformed_traces);
    agg =
      Aggregated_perf_profile.Merge.merge t1.agg t2.agg ~crc_config
        ~ignore_buildid
  }
