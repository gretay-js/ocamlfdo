
module Class : sig
  (** Mapping for all registers on the platform to the frequency structure
    * describing their uses from a program point to any exit points.
    *
    *             A: mov  $1, %rax
    *                  |
    *             Sometimes
    *            /         \
    *           /          \
    * B: add %rax, %rbx     C: mov $2, %rax
    *    Always                Never
    *
    * At program point A, the register is used sometimes since it is never used
    * at program point C (overwritten), but it is always used at B due to the
    * read of the register at that instruction.
    *)
  type t = Path_use.t Register.Map_with_default.t [@@deriving sexp]
end

module Solver : sig
  val solve
    :  Ocamlcfg.Cfg.t
    -> Cfg_info.t option
    -> (Class.t * Class.t) Ocamlcfg.Analysis.Inst_id.Map.t
end
