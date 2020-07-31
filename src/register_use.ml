open Core

module Label = Ocamlcfg.Label
module Analysis = Ocamlcfg.Analysis
module Cfg = Ocamlcfg.Cfg
module BB = Cfg.Basic_block
module Cfg_inst_id = Analysis.Inst_id


module Class = struct
  (** Mapping for all registers on the platform to the frequency structure
    * describing their uses from a program point to any exit points.
    *
    *             mov  $1, %rax
    *                  |
    *             Sometimes
    *            /         \
    *           /          \
    * add %rax, %rbx      mov $2, %rax
    *    Always             Never
    *)
  type t = Path_use.t Register.Map_with_default.t [@@deriving sexp, equal]

  (** For each register, join all paths from this program point *)
  let lub =
    Register.Map_with_default.merge ~f:(fun a b -> Path_use.lub a b)

  (** There are no uses at program exit points. *)
  let never =
    Register.Map_with_default.default (Path_use.Never Frequency.zero)

  (** Internal nodes are initialised to unknown. *)
  let unknown =
    Register.Map_with_default.default (Path_use.Unknown)
end

module Problem = struct
  module K = struct
    module S = Class

    type t =
      { kill: Register.Set.t;
        (* Set of registers written or implicitly destroyed at this instruction. *)
        gen: Register.Set.t;
        (* Registers used in this instruction *)
        freq: Frequency.t
        (* Frequency of the block containing the instruction. *)
      }

    let f (s: Class.t) { kill; gen; freq } =
      (* Removes the killed registers and adds the used registers to the map of
       * live regsiters at this program point. If a register has no uses, the
       * frequency of the 'Never' path is updated.
       *)
      let after_pressure =
        Register.Map_with_default.map s ~f:(fun uses -> Path_use.update_never uses freq)
      in
      let after_kill =
        Register.Set.fold kill ~init:after_pressure ~f:(fun acc key ->
          Register.Map_with_default.set acc ~key ~data:(Path_use.Never freq))
      in
      Register.Set.fold gen ~init:after_kill ~f:(fun acc reg ->
        Register.Map_with_default.set acc ~key:reg ~data:(Path_use.Always freq))

    let dot curr prev =
      { kill = Register.Set.union curr.kill prev.kill;
        gen = Register.Set.union curr.gen (Register.Set.diff prev.gen curr.kill);
        freq = Frequency.max curr.freq prev.freq
      }
  end

  type t = { cfg: Cfg.t; cfg_info: Cfg_info.t option }

  let cfg { cfg; _ } = cfg

  let empty _ _ = Class.unknown

  let entry _ _ = Class.never

  let kg { cfg; cfg_info } inst =
    (* Finds the registers read, writted or implicitly destroyed at the node *)
    let block = Cfg_inst_id.parent inst in
    let bb = Cfg.get_block_exn cfg block in
    let kg inst destroyed =
      let regs_of_array =
        Array.fold ~init:Register.Set.empty ~f:(fun acc reg ->
          match reg.Reg.loc with
          | Reg n -> Register.Set.add acc n
          | _ -> acc)
      in
      let kill =
        Register.Set.union
          (regs_of_array (inst.Cfg.res))
          (Register.Set.of_array destroyed)
      in
      let gen = regs_of_array (inst.Cfg.arg) in
      { K.kill; K.gen; K.freq = Frequency.create cfg_info block }
    in
    match inst with
    | Cfg_inst_id.Term _->
      let t = BB.terminator bb in
      kg t (Cfg.destroyed_at_terminator t.Cfg.desc)
    | Cfg_inst_id.Inst (_, n) ->
      let i = List.nth_exn (BB.body bb) n in
      kg i (Cfg.destroyed_at_instruction i.Cfg.desc)
end

module Solver = struct
  let solve cfg cfg_info =
    let module M = Analysis.Make_backward_cfg_solver(Problem) in
    M.solve { cfg; cfg_info }
end
