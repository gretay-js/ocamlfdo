
module Class : sig
  module Use : sig
    type t =
      { path: Path_use.t;
        pressure: Path_use.t
      } [@@deriving sexp, compare, equal]

    val lub : t -> t -> t
  end

  module Uses : sig
    type t = Path_use.t * Use.t Inst_id.Map.t [@@deriving sexp, compare, equal]
  end

  type t = Uses.t Spill.Map.t [@@deriving sexp]

  val equal : t -> t -> bool

  val lub : t -> t -> t
end

module Solver : sig
  val solve
    :  (Ocamlcfg.Cfg.t * Cfg_info.t option)
    -> (Class.t * Class.t) Ocamlcfg.Analysis.Inst_id.Map.t
end
