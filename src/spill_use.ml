open Core

module Label = Ocamlcfg.Label
module Analysis = Ocamlcfg.Analysis
module Cfg = Ocamlcfg.Cfg
module BB = Cfg.Basic_block
module Cfg_inst_id = Ocamlcfg.Inst_id

let increment = function
  | None -> 1
  | Some n -> n + 1

let count_live_vars inst =
  Reg.Set.fold
    (fun reg acc -> Int.Map.update acc (Proc.register_class reg) ~f:increment)
    inst.Cfg.live
    Int.Map.empty

(** Counts the free registers at a program point.
  * A register is free if it is not implicitly destroyed or live.
  *)
let count_free_regs inst destroyed =
  let registers =
    Array.fold
      destroyed
      ~init:inst.Cfg.live
      ~f:(fun acc r -> Reg.Set.add (Proc.phys_reg r) acc)
  in
  let live_regs =
    Reg.Set.fold
      (fun reg acc ->
        match reg.Reg.loc with
        | Reg.Reg _ -> Int.Map.update acc (Proc.register_class reg) ~f:increment
        | _ -> acc)
      registers
      Int.Map.empty
  in
  Int.Map.mapi live_regs ~f:(fun ~key ~data -> Proc.num_available_registers.(key) - data)

(** Determines if a program point has high register pressure: either there are
  * no free registers or the live varies greatly outnumber physical regs.
  *)
let has_pressure inst destroyed =
  let live_vars = count_live_vars inst in
  let free_regs = count_free_regs inst destroyed in
  let can_allocate = Int.Map.for_all free_regs ~f:(fun r -> r > 0) in
  let low_pressure =
    Int.Map.for_alli
    live_vars
    ~f:(fun ~key ~data ->
        data < int_of_float (float_of_int Proc.num_available_registers.(key) *. 1.2))
  in
  not can_allocate || not low_pressure

module Class = struct
  module Use = struct
    type t =
      { path: Path_use.t;
        pressure: Path_use.t
      } [@@deriving sexp, equal]

    let lub a b =
      { path = Path_use.lub a.path b.path;
        pressure = Path_use.lub a.pressure b.pressure
      }

      let never = { path = Path_use.never; pressure = Path_use.never }

      let bot = { path = Path_use.bot; pressure = Path_use.bot }
  end

  module Uses = struct
    type t =
      { all_uses: Path_use.t;
        reloads: Use.t Inst_id.Map_with_default.t
      } [@@deriving sexp, equal]

    let lub a b =
      { all_uses = Path_use.lub a.all_uses b.all_uses;
        reloads = Inst_id.Map_with_default.merge a.reloads b.reloads ~f:Use.lub
      }

    let never =
      { all_uses = Path_use.never; reloads = Inst_id.Map_with_default.default Use.never }

    let bot =
      { all_uses = Path_use.bot; reloads = Inst_id.Map_with_default.default Use.bot }
  end

  type t = Uses.t Spill.Map_with_default.t [@@deriving sexp, equal]

  let lub a b =
    Spill.Map_with_default.merge a b ~f:Uses.lub

  let never = Spill.Map_with_default.default Uses.never

  let bot = Spill.Map_with_default.default Uses.bot
end

let get_kill_gen block i pressure =
  let kills =
    Array.fold i.Cfg.res ~init:Spill.Set.empty ~f:(fun kills reg ->
      match reg with
      | { loc = Reg.Stack (Reg.Local s); _} ->
        Spill.Set.add kills (Proc.register_class reg, s)
      | _ -> kills)
  in
  let gens =
    Array.fold i.Cfg.arg ~init:Spill.Map.empty ~f:(fun gens reg ->
      match reg with
      | { loc = Reg.Stack (Reg.Local s); _} ->
        let id = block, i.Cfg.id in
        Spill.Map.set gens
          ~key:(Proc.register_class reg, s)
          ~data:(Inst_id.Map.singleton id pressure)
      | _ -> gens)
  in
  kills, gens

module Problem = struct
  open Class
  open Uses
  open Use

  module A = struct
    module S = Class

    module G = struct
      (** Kill-Gen information for individual instructions which can be aggregated
        * to represent an entire basic block throug composition
        *)
      type t =
        { kills: Spill.Set.t
        (* Set of spill slots overwritten at any point. *)
        ; gens: bool Inst_id.Map.t Spill.Map.t
        (** Set of spill slots read at any point which also reach the first instruction,
          * i.e. are not overwritten by preceding instructions.
          * The flag indicates whether there is register pressure between the first
          * instruction and the instruction using the spill slot.
          *)
        ; pressure: bool
        (* Indicates whether there is register pressure at any of the aggregated points *)
        ; freq: Frequency.t
        (* The execution frequency of the block or the instruction. *)
        }

      let dot curr prev =
        { kills = Spill.Set.union curr.kills prev.kills
        ; gens =
          Spill.Map.merge prev.gens curr.gens ~f:(fun ~key:_ v->
            match v with
            | `Both(prev_reloads, curr_reloads) ->
              Some (Inst_id.Map.merge prev_reloads curr_reloads ~f:(fun ~key v ->
                match v with
                | `Both(_, curr) -> Some curr
                | `Left _ when Spill.Set.mem curr.kills key -> None
                | `Left a -> Some (a || curr.pressure)
                | `Right b -> Some b))
            | `Left a -> Some a
            | `Right b -> Some b)
        ; pressure = curr.pressure || prev.pressure
        ; freq = Frequency.max curr.freq prev.freq
        }
    end

    let apply s { G.kills; gens; pressure; freq } =
      let after_pressure =
        Spill.Map_with_default.map s ~f:(fun { all_uses; reloads }->
          let reloads =
            Inst_id.Map_with_default.map reloads ~f:(fun p ->
              { path = Path_use.update_never p.path freq
              ; pressure = if pressure then Path_use.Always freq else p.pressure
              })
          in
          { all_uses = Path_use.update_never all_uses freq; reloads })
      in
      let after_kill =
        Spill.Set.fold kills ~init:after_pressure ~f:(fun acc key ->
          Spill.Map_with_default.set acc ~key ~data:Class.Uses.never)
      in
      Spill.Map.fold gens ~init:after_kill ~f:(fun ~key:spill_key ~data acc ->
        Spill.Map_with_default.update acc spill_key ~f:(fun { reloads; _ } ->
          let reloads =
            Inst_id.Map.fold data ~init:reloads ~f:(fun ~key:reload_key ~data acc ->
              Inst_id.Map_with_default.update acc reload_key ~f:(fun prev ->
                { path = Path_use.Always freq;
                  pressure =
                    (* data indicates whether there is register pressure between
                     * block entry and the program point where the spill slot is
                     * used by the reload.
                     * If there is register pressure, set the use to 'Always'.
                     * If there is no pressure and the slot is not live-in to the
                     * instruction generating the use, there is no pressure to combine
                     * with, so use 'Never'.
                     * If the slot is live-in to the block without pressure, combine.
                     *)
                    if data then Path_use.Always freq
                    else if Spill.Set.mem kills spill_key then Path_use.Never freq
                    else Path_use.lub (Path_use.Always freq) prev.pressure
                }))
          in
          { all_uses = Path_use.Always freq; reloads }))

  end

  type t = { cfg: Cfg.t; cfg_info: Cfg_info.t option }

  let cfg { cfg; _ } = cfg

  let entry _ _ = Class.never

  let action { cfg; cfg_info; _ } inst =
    let block = Cfg_inst_id.parent inst in
    let freq = Frequency.create cfg_info block in
    let pressure, (kills, gens) =
      match Cfg_inst_id.get_inst cfg inst with
      | `Term t ->
        let pressure = has_pressure t (Cfg.destroyed_at_terminator t.desc) in
        pressure, get_kill_gen block t pressure
      | `Basic i ->
        let pressure = has_pressure i (Cfg.destroyed_at_basic i.desc) in
        pressure, get_kill_gen block i pressure
    in
    A.G.{ kills; gens; pressure; freq }

end

module Solver = struct
  let solve cfg cfg_info =
    let module M = Analysis.Make_backward_cfg_solver(Problem) in
    M.solve { cfg; cfg_info }
end
