
type t
  = Never of Frequency.t
  | Always of Frequency.t
  | Sometimes of Frequency.t * Frequency.t
  [@@deriving sexp, equal, compare]

let lub a b =
  match a, b with
  | Never fa, Never fb ->
    Never (Frequency.lub fa fb)
  | Never fa, Always fb->
    Sometimes (fb, fa)
  | Never fa, Sometimes (fba, fbn) ->
    Sometimes (fba, Frequency.lub fa fbn)
  | Always fa, Never fb ->
    Sometimes (fa, fb)
  | Always fa, Always fb->
    Always (Frequency.lub fa fb)
  | Always fa, Sometimes (fba, fbn) ->
    Sometimes (Frequency.lub fa fba, fbn)
  | Sometimes (faa, fan), Never fb ->
    Sometimes (faa, Frequency.lub fan fb)
  | Sometimes (faa, fan), Always fb->
    Sometimes (Frequency.lub faa fb, fan)
  | Sometimes (faa, fan), Sometimes (fba, fbn) ->
    Sometimes (Frequency.lub faa fba, Frequency.lub fan fbn)

let never = Never Frequency.zero

let update_never v freq =
  match v with
  | Never f -> Never (Frequency.lub f freq)
  | _ -> v

