open Core

type 'a interval = {
  l : Addr.t;
  r : Addr.t;
  v : 'a;
}

(* Interval's lower bound is the key. *)
type 'a t = 'a interval Map.M(Addr).t

let empty = Map.empty (module Addr)

let _verbose = true

let enclosing t a =
  match Map.closest_key t `Less_or_equal_to a with
  | None -> None
  | Some (_, interval) ->
      let open Addr in
      assert (interval.l <= a);
      if a < interval.r then Some interval else None

let _print msg = function
  | None -> printf "%s: none\n" msg
  | Some i -> printf "%s: [0x%Lx, 0x%Lx]\n" msg i.l i.r

(* Checks whether [i] is disjoint from all the intervals in t. *)
let disjoint t i =
  (* Find intervals in [t] that are immediately below and above [i.l], and
     check their boundaries: below.r <= i.l and i.r <= above.l *)

  (* use functions not a variables for short-circuit evaluation *)
  let check_below () =
    match Map.closest_key t `Less_or_equal_to i.l with
    | None -> true
    | Some (_, below) ->
        let open Addr in
        assert (below.l <= i.l);
        if below.r <= i.l then true else false
  in
  let check_above () =
    match Map.closest_key t `Greater_than i.l with
    | None -> true
    | Some (_, above) ->
        let open Addr in
        assert (i.l <= above.l);
        if i.r <= above.l then true else false
  in
  check_below () && check_above ()

let insert t interval =
  (* Non-empty intervals only *)
  assert Int64.(interval.l < interval.r);

  (* Check that the new interval is disjoint from all existing intervals in
     [t] *)
  assert (disjoint t interval);
  Map.add_exn t ~key:interval.l ~data:interval
