
(**
 * Represents the use of all spill slots at a program point.
 *)
module Class : sig
  (** Represents a single user of a stack slot live at the program point. *)
  module Use : sig
    type t =
      { path: Path_use.t;
        (** Identifies whether the spill slot uses the value on all or only
          * on some paths between the program point and the exit.
          *)
        pressure: Path_use.t
        (** Identifies whether register pressure is present on all or only
          * on some paths to the user of the stack slot.
          *)
      } [@@deriving sexp, equal]
  end

  (** Aggreggates all users of a stack slot at the program point. *)
  module Uses : sig
    type t =
      { all_uses: Path_use.t;
        (** Aggregates all users of the spill slot, identifying whether
          * it is used on all or only on some paths to the exit.
          *)
        reloads: Use.t Inst_id.Map_with_default.t
        (** Classification specific for each reached user of the slot. *)
      } [@@deriving sexp, equal]
  end

  (** Mapping from all spill slots to their classification at a point. *)
  type t = Uses.t Spill.Map_with_default.t [@@deriving sexp, equal]
end

module Solver : sig
  val solve
    :  Ocamlcfg.Cfg.t
    -> Cfg_info.t option
    -> (Class.t * Class.t) Ocamlcfg.Analysis.Inst_id.Map.t
end
