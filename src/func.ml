open Core

type t =
  { id : int
  ; start : Raw_addr.t
  ; finish : Raw_addr.t
  ; mutable count : Execount.t
  ; mutable has_linearids : bool
  ; agg : Aggregated_perf_profile.t
  }
[@@deriving sexp, bin_io]

let mk ~id ~start ~finish =
  { id
  ; start
  ; finish
  ; has_linearids = false
  ; count = 0L
  ; agg = Aggregated_perf_profile.empty ()
  }
;;

let merge t1 t2 ~crc_config ~ignore_buildid =
  if not
       (t1.id = t2.id
       && ((Raw_addr.equal t1.start t2.start && Raw_addr.equal t1.finish t2.finish)
          || ignore_buildid))
  then Report.user_error !"Cannot merge functions:\n%{sexp:t}\n%{sexp:t}\n" t1 t2;
  { id = t1.id
  ; start = t1.start
  ; finish = t1.finish
  ; has_linearids = t1.has_linearids || t2.has_linearids
  ; count = Execount.(t1.count + t2.count)
  ; agg = Aggregated_perf_profile.Merge.merge t1.agg t2.agg ~crc_config ~ignore_buildid
  }
;;
