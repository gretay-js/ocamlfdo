open Core

module Label = Ocamlcfg.Label
module Analysis = Ocamlcfg.Analysis
module Cfg = Ocamlcfg.Cfg
module BB = Cfg.Basic_block
module Cfg_inst_id = Analysis.Inst_id


module Class = struct
  (** Mapping for all registers on the platform to the frequency structure
    * describing their uses from a program point to any exit points.
    *)
  type t = Path_use.t Register.Map.t [@@deriving sexp]

  let equal = Register.Map.equal Path_use.equal

  let lub =
    (* For each register, join all paths from this program point *)
    Register.Map.merge ~f:(fun ~key:_ v ->
      match v with
      | `Both(a, b) -> Some (Path_use.lub a b)
      | `Left a -> Some a
      | `Right b -> Some b)
end

let all_unused_registers =
  (* Build a map mapping never to all physical regs *)
  Register.Set.fold
    Register.all_registers
    ~init:Register.Map.empty
    ~f:(fun acc reg ->
        Register.Map.set acc ~key:reg ~data:(Path_use.Never Execount.zero))

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

    let f s { kill; gen; freq } =
      (* Removes the killed registers and adds the used registers to the map of
       * live regsiters at this program point. If a register has no uses, the
       * frequency of the 'Never' path is updated.
       *)
      let open Path_use in
      let not_killed =
        Register.Map.mapi s ~f:(fun ~key ~data ->
          if Register.Set.mem kill key
            then Path_use.Never freq
            else Path_use.update_never data freq)
      in
      Register.Set.fold gen ~init:not_killed ~f:(fun acc reg ->
        Register.Map.set acc ~key:reg ~data:(Always freq))

    let dot curr prev =
      { kill = Register.Set.union curr.kill prev.kill;
        gen = Register.Set.union curr.gen (Register.Set.diff prev.gen curr.kill);
        freq = Frequency.max curr.freq prev.freq
      }
  end

  type t = { cfg: Cfg.t; cfg_info: Cfg_info.t option }

  let cfg { cfg; _ } = cfg

  let init { cfg; _ } block =
    (* Initialises program exit points, setting register paths to 'Never' *)
    let bb = Cfg.get_block_exn cfg block in
    if BB.is_exit bb then
      (all_unused_registers, Register.Map.empty)
    else
      (Register.Map.empty, Register.Map.empty)

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
