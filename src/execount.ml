let ignore_overflow = ref false
let overflow_detected = ref 0

let report_overflow () =
  if !overflow_detected > 0
  then
    Report.logf
      "Overflow in execution counter aggregation detected %d times. Used max_value."
      !overflow_detected
;;

include Core.Int64

(* assumes that execution counters are always non-negative. *)
let add x y =
  (* CR-someday gyorsh: more efficent way to detect overflow. *)
  let c = x + y in
  if x >= 0L && y >= 0L && c >= 0L
  then c
  else if !ignore_overflow
  then (
    Caml.incr overflow_detected;
    max_value)
  else
    Report.user_error
      "Overflow in execution counter aggregation. Use command line option \
       -ignore-overflow."
;;

(* raise Overflow *)

let ( + ) x y = add x y
