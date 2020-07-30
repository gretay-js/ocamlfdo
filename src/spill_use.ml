open Core

module Label = Ocamlcfg.Label
module Analysis = Ocamlcfg.Analysis
module Cfg = Ocamlcfg.Cfg
module BB = Cfg.Basic_block
module Cfg_inst_id = Analysis.Inst_id

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

      let unknown = { path = Path_use.unknown; pressure = Path_use.unknown }
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

    let unknown =
      { all_uses = Path_use.unknown; reloads = Inst_id.Map_with_default.default Use.unknown }
  end

  type t = Uses.t Spill.Map_with_default.t [@@deriving sexp, equal]

  let lub a b =
    Spill.Map_with_default.merge a b ~f:Uses.lub

  let never = Spill.Map_with_default.default Uses.never

  let unknown = Spill.Map_with_default.default Uses.unknown
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

  module K = struct
    module S = Class

    type t =
      { kills: Spill.Set.t
      ; gens: bool Inst_id.Map.t Spill.Map.t
      ; pressure: bool
      ; freq: Frequency.t
      }

    let f s { kills; gens; pressure; freq } =
      let after_kill =
        Spill.Set.fold kills ~init:s ~f:(fun acc key ->
          Spill.Map_with_default.set acc ~key ~data:Class.Uses.never)
      in
      let after_pressure =
        Spill.Map_with_default.map after_kill ~f:(fun { all_uses; reloads }->
          let reloads =
            Inst_id.Map_with_default.map reloads ~f:(fun p ->
              { path = Path_use.update_never p.path freq
              ; pressure = if pressure then Path_use.Always freq else p.pressure
              })
          in
          { all_uses = Path_use.update_never all_uses freq; reloads })
      in
      Spill.Map.fold gens ~init:after_pressure ~f:(fun ~key ~data acc ->
        Spill.Map_with_default.update acc key ~f:(fun { reloads; _ } ->
          let reloads =
            Inst_id.Map.fold data ~init:reloads ~f:(fun ~key ~data acc ->
              Inst_id.Map_with_default.update acc key ~f:(fun prev ->
                { path = Path_use.Always freq;
                  pressure =
                    if data then Path_use.Always freq
                    else
                      match prev.path with
                      | Unknown | Never _ -> Path_use.Never freq
                      | Sometimes _ | Always _ -> prev.pressure
                }))
          in
          { all_uses = Path_use.Always freq; reloads }))

    let dot curr prev =
      let prev_gens =
        Spill.Map.filter_mapi prev.gens ~f:(fun ~key ~data ->
          if Spill.Set.mem curr.kills key then None
          else Some (Inst_id.Map.map data ~f:(fun pressure -> pressure || curr.pressure)))
      in
      { kills = Spill.Set.union curr.kills prev.kills
      ; gens =
        Spill.Map.merge prev_gens curr.gens ~f:(fun ~key:_ v->
          match v with
          | `Both(prev_reloads, curr_reloads) ->
            Some (Inst_id.Map.merge prev_reloads curr_reloads ~f:(fun ~key:_ v ->
              match v with
              | `Both(_prev, _curr) -> Some _curr
              | `Left a -> Some a
              | `Right b -> Some b))
          | `Left a -> Some a
          | `Right b -> Some b)
      ; pressure = curr.pressure || prev.pressure
      ; freq = curr.freq
      }
  end

  type t = { cfg: Cfg.t; cfg_info: Cfg_info.t option }

  let cfg { cfg; _ } = cfg

  let init { cfg; _ } block =
    let bb = Cfg.get_block_exn cfg block in
    ((if BB.is_exit bb then Class.never else Class.unknown), Class.unknown)

  let kg { cfg; cfg_info; _ } inst =
    let block = Cfg_inst_id.parent inst in
    let bb = Cfg.get_block_exn cfg block in
    let freq = Frequency.create cfg_info block in
    let pressure, (kills, gens) =
      match inst with
      | Cfg_inst_id.Term _ ->
        let t = BB.terminator bb in
        let pressure = has_pressure t (Cfg.destroyed_at_terminator t.desc) in
        pressure, get_kill_gen block t pressure
      | Cfg_inst_id.Inst (_, n) ->
        let i = List.nth_exn (BB.body bb) n in
        let pressure = has_pressure i (Cfg.destroyed_at_instruction i.desc) in
        pressure, get_kill_gen block i pressure
    in
    K.{ kills; gens; pressure; freq }

end

module Solver = struct
  let solve cfg cfg_info =
    let module M = Analysis.Make_backward_cfg_solver(Problem) in
    M.solve { cfg; cfg_info }
end
