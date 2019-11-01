open Core
include Int

(* break abstraction and dig in the guts of owee and addr both and match
   them up. *)
let mk = Int64.to_int_exn

let get = Int64.of_int
