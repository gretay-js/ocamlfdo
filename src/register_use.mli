
module Class : sig
  (** Mapping for all registers on the platform to the frequency structure
    * describing their uses from a program point to any exit points.
    *)
  type t = Path_use.t Register.Map.t [@@deriving sexp]
end

module Solver : sig
  val solve
    :  Ocamlcfg.Cfg.t
    -> Cfg_info.t option
    -> (Class.t * Class.t) Ocamlcfg.Analysis.Inst_id.Map.t
end
