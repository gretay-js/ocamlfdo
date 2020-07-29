
(* Describes how frequently a value is used on a path
 * from a program point to any exit points, including
 * the execution count of the heaviest user on the path.
 *)
type t
  = Unknown
  (* No information is known about the use of the value. *)
  | Never of Frequency.t
  (* The value was never used on any path from a program point
     to the exit. The frequency is the execution count of the
     most executed block on this path. *)
  | Always of Frequency.t
  (* The value is used on all paths from a program point to
     any of the exists. Carries the count of the most frequently
     executed user on the path. *)
  | Sometimes of Frequency.t * Frequency.t
  (* Joins two paths, carrying the maximal execution count of the
     most frequent user and the maximal execution count
     on a path which never uses the value. *)
  [@@deriving sexp, equal, compare]

val lub : t -> t -> t

val max : t -> t -> t
(* Rename this method *)

val never : t
(* No use with no frequency count *)

val unknown : t
(* Unknown value *)

val update_never : t -> Frequency.t -> t
(* If the value is never, it updates the frequency if it is higher *)
