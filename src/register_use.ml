open Core

module Label = Ocamlcfg.Label
module Analysis = Ocamlcfg.Analysis
module Cfg = Ocamlcfg.Cfg
module BB = Cfg.Basic_block
module Cfg_inst_id = Ocamlcfg.Inst_id


module Class = struct
  type t = Path_use.t Register.Map_with_default.t [@@deriving sexp, equal]

  (** For each register, join all paths from this program point *)
  let lub =
    Register.Map_with_default.merge ~f:(fun a b -> Path_use.lub a b)

  (** There are no uses at program exit points. *)
  let never =
    Register.Map_with_default.default (Path_use.Never Frequency.zero)

  (** Internal nodes are initialised to unknown. *)
  let bot =
    Register.Map_with_default.default (Path_use.Unknown)
end

module Problem = struct
  module A = struct
    module S = Class

    module G = struct
      type t =
        { kill: Register.Set.t;
          (* Set of registers written or implicitly destroyed at this instruction. *)
          gen: Register.Set.t;
          (* Registers used in this instruction *)
          freq: Frequency.t
          (* Frequency of the block containing the instruction. *)
        }
      let dot curr prev =
        { kill = Register.Set.union curr.kill prev.kill;
          gen = Register.Set.union curr.gen (Register.Set.diff prev.gen curr.kill);
          freq = Frequency.max curr.freq prev.freq
        }
    end

    let apply (s: Class.t) { G.kill; gen; freq } =
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

  end

  type t = { cfg: Cfg.t; cfg_info: Cfg_info.t option }

  let cfg { cfg; _ } = cfg

  let entry _ _ = Class.never

  let action { cfg; cfg_info } inst =
    (* Finds the registers read, writted or implicitly destroyed at the node *)
    let block = Cfg_inst_id.parent inst in
    let action inst destroyed =
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
      { A.G.kill; gen; freq = Frequency.create cfg_info block }
    in
    match Cfg_inst_id.get_inst cfg inst with
    | `Term t -> action t (Cfg.destroyed_at_terminator t.Cfg.desc)
    | `Basic i -> action i (Cfg.destroyed_at_basic i.Cfg.desc)
end

module Solver = struct
  let solve cfg cfg_info =
    let module M = Analysis.Make_backward_cfg_solver(Problem) in
    M.solve { cfg; cfg_info }
end
