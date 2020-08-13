
(** Describes how frequently a value is used on a path
  * from a program point to any exit points, including
  * the execution count of the heaviest user on the path.
  *)
type t
  = Unknown
  (** No information is known about the use of the value.
    * This vaulue should be used to initialise analyses.
    *)
  | Never of Frequency.t
  (** The value was never used on any path from a program point
    * to the exit. The frequency is the execution count of the
    * most executed block on any of these paths.
    *)
  | Always of Frequency.t
  (** The value is used on all paths from a program point to
    * any of the exists. Carries the count of the most frequently
    * executed user on any of the paths.
    *)
  | Sometimes of Frequency.t * Frequency.t
  (** Joins two paths, carrying the maximal execution count of the
    * most frequent user and the maximal execution count
    * on a path which never uses the value.
    *)
  [@@deriving sexp, equal, compare]

(* Least Upper Bound of two lattice elements. *)
val lub : t -> t -> t

(* Bottom value of the lattice - unknown *)
val bot : t

(** Picks the most frequently used path from the uses of multiple
  * values over overlapping paths. *)
val max : t -> t -> t

(* Checks if the path_use is an Always. *)
val is_always : t -> bool

(* Checks if the path is more biased to the never component. *)
val is_less_frequent : t -> bool

(* No use with no frequency count *)
val never : t

(* If the value is Never, updates the frequency if it is higher *)
val update_never : t -> Frequency.t -> t
