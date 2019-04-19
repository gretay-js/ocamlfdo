
(** Linear to cfg and back. Apply transformation [f] to cfg. *)
val fundecl
  : Linearize.fundecl
  -> transform:(Cfg.t -> Cfg.t)
  -> validate:bool
  -> Linearize.fundecl
