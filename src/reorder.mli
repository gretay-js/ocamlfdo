
(* Contains a permutation of the original linear ids. *)
type fun_layout = (int, int) Hashtbl.t
(* Maps functions to layout of the function. Spares, i.e., only contains
functions whose layout changed. *)
type layout = (string, fun_layout) Hashtbl.t
