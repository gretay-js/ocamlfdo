(* Intervals data structure that stores disjoint intervals. Supports
   efficient add and find (assuming that map's find is efficient). We don't
   need an interval tree, because all intervals are disjoint.

   The interval's start is closed and interval's end is open: [i.l,i.r) *)

type 'a interval = {
  l : Addr.t;
  r : Addr.t;
  v : 'a;
}

type 'a t

val empty : 'a t

val insert : 'a t -> 'a interval -> 'a t

val enclosing : 'a t -> Addr.t -> 'a interval option
