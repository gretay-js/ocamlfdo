
type t
  = Unknown
  | Never of Frequency.t
  | Always of Frequency.t
  | Sometimes of Frequency.t * Frequency.t
  [@@deriving sexp, equal, compare]

let lub a b =
  match a, b with
  | Unknown, _ -> b
  | _, Unknown -> a
  | Never never_a, Never never_b ->
    Never (Frequency.lub never_a never_b)
  | Never never, Always always->
    Sometimes (always, never)
  | Never never, Sometimes (always_b, never_b) ->
    Sometimes (always_b, Frequency.lub never never_b)
  | Always always, Never never ->
    Sometimes (always, never)
  | Always always_a, Always always_b->
    Always (Frequency.lub always_a always_b)
  | Always always, Sometimes (always_a, never_b) ->
    Sometimes (Frequency.lub always always_a, never_b)
  | Sometimes (always_a, never_a), Never never_b ->
    Sometimes (always_a, Frequency.lub never_a never_b)
  | Sometimes (always_a, never_a), Always always_b->
    Sometimes (Frequency.lub always_a always_b, never_a)
  | Sometimes (always_a, never_a), Sometimes (always_b, never_b) ->
    Sometimes (Frequency.lub always_a always_b, Frequency.lub never_a never_b)

let max a b =
  match a, b with
  | Unknown, _ -> b
  | _, Unknown -> a
  | Always fa, Always fb -> Always (Frequency.max fa fb)
  | Always _, _ -> a
  | _, Always _ -> b
  | Never fa, Never fb -> Never (Frequency.max fa fb)
  | Never _, _ -> b
  | _, Never _ -> a
  | Sometimes (aa, an), Sometimes(ba, bn) ->
    Sometimes (Frequency.max aa ba, Frequency.max an bn)

let is_always = function
  | Always _ -> true
  | _ -> false

let never = Never Frequency.zero

let unknown = Unknown

let update_never v freq =
  match v with
  | Never f -> Never (Frequency.lub f freq)
  | Sometimes (always, never) -> Sometimes (always, Frequency.lub never freq)
  | _ -> v

