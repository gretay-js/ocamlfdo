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

let count_free_regs inst destroyed =
  (* Counts the free registers at a program point.
   * A register is free if it is not implicitly destroyed or live here.
   *)
  let registers =
    Array.fold
      destroyed
      ~init:inst.Cfg.live
      ~f:(fun acc r -> Reg.Set.add (Proc.phys_reg r) acc)
  in
  let live_regs =
    Reg.Set.fold
      (fun reg acc ->
        let cls = Proc.register_class reg in
        match reg.Reg.loc with
        | Reg.Reg _ -> Int.Map.update acc cls ~f:increment
        | _ -> acc)
      registers
      Int.Map.empty
  in
  Int.Map.mapi live_regs ~f:(fun ~key ~data -> Proc.num_available_registers.(key) - data)

let has_pressure inst destroyed =
  (* Determines if a program point has high register pressure: either there are
     no free registers or the live varies greatly outnumber physical regs. *)
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
      }
      [@@deriving sexp, compare, equal]

    let lub a b =
      { path = Path_use.lub a.path b.path;
        pressure = Path_use.lub a.pressure b.pressure
      }
  end

  module Uses = struct
    type t
      = Path_use.t * Use.t Inst_id.Map.t
      [@@deriving sexp, compare, equal]
  end

  type t = Uses.t Spill.Map.t [@@deriving sexp]

  let equal = Spill.Map.equal Uses.equal

  let lub a b =
    Spill.Map.merge a b ~f:(fun ~key:_ v ->
      match v with
      | `Both ((fa, ra), (fb, rb)) ->
        let f = Path_use.lub fa fb in
        let r =
          Inst_id.Map.merge ra rb ~f:(fun ~key:_ v ->
            match v with
            | `Both(rca, rcb) ->
              Some (Use.lub rca rcb)
            | `Left a -> Some a
            | `Right b -> Some b)
        in
        Some (f, r)
      | `Left a -> Some a
      | `Right b -> Some b)

  let all_unused_reloads cfg slot =
    Inst_id.Set.fold
      (Spill.all_reloads cfg slot)
      ~init:Inst_id.Map.empty
      ~f:(fun acc key ->
          let open Use in
          let data = { path = Path_use.never; pressure = Path_use.never } in
          Inst_id.Map.set acc ~key ~data)

  let all_unused_spills cfg =
    Spill.Set.fold
      (Spill.all_spills cfg)
      ~init:Spill.Map.empty
      ~f:(fun acc key ->
          let data = Path_use.never, all_unused_reloads cfg key in
          Spill.Map.set acc ~key ~data)
end

module Problem = struct
  module K = struct
    module S = Class

    type t =
      { kills: Spill.Set.t
      ; gens: Class.Use.t Inst_id.Map.t Spill.Map.t
      ; pressure: bool
      ; freq: Frequency.t
      }

    let f s { kills; gens; pressure; freq } =
      let not_killed =
        Spill.Map.mapi s ~f:(fun ~key ~data ->
          let open Class.Use in
          let use_freq, reloads = data in
          if Spill.Set.mem kills key
            then
              let reloads' =
                Inst_id.Map.map reloads ~f:(fun _ ->
                    { path = Path_use.never; pressure = Path_use.never })
              in
              (Path_use.Never freq, reloads')
            else
              let reloads' =
                Inst_id.Map.map reloads ~f:(fun p ->
                  let spill_use =
                    { path = Path_use.update_never p.path freq
                    ; pressure = if pressure then Path_use.Always freq else p.pressure
                    }
                  in
                  spill_use)
              in
              (Path_use.update_never use_freq freq, reloads'))
      in
      Spill.Map.fold gens ~init:not_killed ~f:(fun ~key ~data acc ->
        Spill.Map.update acc key ~f:(function
          | None -> (Path_use.Always freq, data)
          | Some (_, reloads) ->
            let reloads' =
              Inst_id.Map.merge reloads data ~f:(fun ~key:_ v ->
                match v with
                | `Both(_, r) -> Some r
                | `Left a -> Some a
                | `Right b -> Some b)
            in
            (Path_use.Always freq, reloads')))

    let dot curr prev =
      let prev_gens =
        Spill.Map.filter_mapi prev.gens ~f:(fun ~key ~data ->
          if Spill.Set.mem curr.kills key then None
          else
            let open S.Use in
            Some (Inst_id.Map.map data ~f:(fun p ->
              let pressure =
                if curr.pressure
                  then Path_use.Always curr.freq
                  else p.pressure
              in
              { p with pressure = pressure })))
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

  type t = { cfg: Cfg.t; cfg_info: Cfg_info.t option; all_unused: Class.t }

  let cfg { cfg; _ } = cfg

  let init { cfg; all_unused; _ } block =
    let bb = Cfg.get_block_exn cfg block in
    if BB.is_exit bb then
      (all_unused, Spill.Map.empty)
    else
      (Spill.Map.empty, Spill.Map.empty)

  let kg { cfg; cfg_info; _ } inst =
    let block = Cfg_inst_id.parent inst in
    let bb = Cfg.get_block_exn cfg block in
    let freq = Frequency.create cfg_info block in
    let open K in
    match inst with
    | Cfg_inst_id.Term _ ->
      let term = BB.terminator bb in
      { kills = Spill.Set.empty
      ; gens = Spill.Map.empty
      ; pressure = has_pressure term (Cfg.destroyed_at_terminator term.desc)
      ; freq
      }
    | Cfg_inst_id.Inst (_, n) ->
      let i = List.nth_exn (BB.body bb) n in
      let pressure = has_pressure i (Cfg.destroyed_at_instruction i.desc) in
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
            let open Class in
            let spill_use =
                { Use.path = Path_use.Always freq
                ; pressure =
                  if pressure
                    then Path_use.Always freq
                    else Path_use.Never freq
                }
            in
            let id = block, i.Cfg.id in
            Spill.Map.set gens
              ~key:(Proc.register_class reg, s)
              ~data:(Inst_id.Map.singleton id spill_use)
          | _ -> gens)
      in
      { kills; gens; pressure; freq }
end

module Solver = struct
  let solve cfg cfg_info =
    let module M = Analysis.Make_backward_cfg_solver(Problem) in
    let all_unused =  Class.all_unused_spills cfg in
    M.solve { cfg; cfg_info; all_unused }
end
