
module Class : sig
  module Use : sig
    type t =
      { path: Path_use.t;
        pressure: Path_use.t
      } [@@deriving sexp, equal]

    val lub : t -> t -> t
  end

  module Uses : sig
    type t =
      { all_uses: Path_use.t;
        reloads: Use.t Inst_id.Map_with_default.t
      } [@@deriving sexp, equal]

    val lub : t -> t -> t
  end

  type t = Uses.t Spill.Map_with_default.t [@@deriving sexp, equal]

  val lub : t -> t -> t
end

module Solver : sig
  val solve
    :  Ocamlcfg.Cfg.t
    -> Cfg_info.t option
    -> (Class.t * Class.t) Ocamlcfg.Analysis.Inst_id.Map.t
end
