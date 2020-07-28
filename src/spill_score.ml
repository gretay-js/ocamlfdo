open Core

module Label = Ocamlcfg.Label
module Analysis = Ocamlcfg.Analysis
module CL = Ocamlcfg.Cfg_with_layout
module Cfg = Ocamlcfg.Cfg
module BB = Cfg.Basic_block
module AD = Aggregated_decoded_profile
module Cfg_inst_id = Analysis.Inst_id

module Frequency = struct
  include Execount

  let create cfg_info block =
    match cfg_info with
    | None -> zero
    | Some cfg_info ->
      match Cfg_info.get_block cfg_info block with
      | None -> zero
      | Some bf -> bf.count

  let lub = Execount.max
end

module Path_use = struct
  (* Describes how frequently a value is used on a path
   * from a program point to any exit points, including
   * the execution count of the heaviest user on the path.
   *)
  type t
    = Never of Frequency.t
    (* The value was never used on any path from a program point
       to the exit. The frequency is the execution count of the
       most executed block on this path. *)
    | Always of Frequency.t
    (* The value is used on all paths from a program point to
       any of the exists. Carries the count of the most frequently
       executed user on the path. *)
    | Sometimes of Frequency.t * Frequency.t
    (* Joins two paths, carrying the maximal execution count of the
       most frequent user and the maximal execution count
       on a path which never uses the value. *)
    [@@deriving sexp, equal, compare]

  let lub a b =
    match a, b with
    | Never fa, Never fb ->
      Never (Frequency.lub fa fb)
    | Never fa, Always fb->
      Sometimes (fb, fa)
    | Never fa, Sometimes (fba, fbn) ->
      Sometimes (fba, Frequency.lub fa fbn)
    | Always fa, Never fb ->
      Sometimes (fa, fb)
    | Always fa, Always fb->
      Always (Frequency.lub fa fb)
    | Always fa, Sometimes (fba, fbn) ->
      Sometimes (Frequency.lub fa fba, fbn)
    | Sometimes (faa, fan), Never fb ->
      Sometimes (faa, Frequency.lub fan fb)
    | Sometimes (faa, fan), Always fb->
      Sometimes (Frequency.lub faa fb, fan)
    | Sometimes (faa, fan), Sometimes (fba, fbn) ->
      Sometimes (Frequency.lub faa fba, Frequency.lub fan fbn)

  let never = Never Frequency.zero
end

module Register = struct
  (* Less insane wrapper around the hardware register
   * numbers used by the Linear IR
   *)
  module T = struct
    type t = int [@@deriving sexp, compare]
  end

  include T
  module Set = Set.Make(T)
  module Map = Map.Make(T)

  let all_registers =
    (* Set of all available registers on the target *)
    let regs = ref Set.empty in
    for i = 0 to Proc.num_register_classes - 1 do
      for j = 0 to Proc.num_available_registers.(i) - 1 do
        regs := Set.add !regs (Proc.first_available_register.(i) + j)
      done
    done;
    !regs
end

module Register_use_class = struct
  (** Mapping for all registers on the platform to the frequency structure
    * describing their uses from a program point to any exit points.
    *)
  type t = Path_use.t Register.Map.t [@@deriving sexp]

  let equal = Register.Map.equal Path_use.equal

  let lub =
    Register.Map.merge ~f:(fun ~key:_ v ->
      match v with
      | `Both(a, b) -> Some (Path_use.lub a b)
      | `Left a -> Some a
      | `Right b -> Some b)

  let all_unused_registers =
    Register.Set.fold
      Register.all_registers
      ~init:Register.Map.empty
      ~f:(fun acc reg ->
          Register.Map.set acc ~key:reg ~data:(Path_use.Never Execount.zero))
end

let update_never freq = function
  | Path_use.Never f -> Path_use.Never (Frequency.lub f freq)
  | v -> v

module Reg_use_classify_problem = struct
  module K = struct
    module S = Register_use_class

    type t =
      { kill: Register.Set.t
      ; gen: Register.Set.t
      ; freq: Frequency.t
      }

    let f s { kill; gen; freq } =
      let open Path_use in
      let not_killed =
        Register.Map.mapi s ~f:(fun ~key ~data ->
          if Register.Set.mem kill key
            then Path_use.Never freq
            else update_never freq data)
      in
      Register.Set.fold gen ~init:not_killed ~f:(fun acc reg ->
        Register.Map.set acc ~key:reg ~data:(Always freq))

    let dot curr prev =
      { kill = Register.Set.union curr.kill prev.kill
      ; gen = Register.Set.union curr.gen (Register.Set.diff prev.gen curr.kill)
      ; freq = Frequency.max curr.freq prev.freq
      }
  end

  type t =
    { cfg: Cfg.t
    ; cfg_info: Cfg_info.t option
    }

  let cfg { cfg; _ } = cfg

  let init { cfg; _ } block =
    let bb = Cfg.get_block_exn cfg block in
    if BB.is_exit bb then
      (Register_use_class.all_unused_registers, Register.Map.empty)
    else
      (Register.Map.empty, Register.Map.empty)

  let kg { cfg; cfg_info } inst =
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

module Reg_use_classify = Analysis.Make_backward_cfg_solver(Reg_use_classify_problem)

let increment = function
  | None -> 1
  | Some n -> n + 1

let count_live_vars inst =
  Reg.Set.fold
    (fun reg acc -> Int.Map.update acc (Proc.register_class reg) ~f:increment)
    inst.Cfg.live
    Int.Map.empty

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
        let cls = Proc.register_class reg in
        match reg.Reg.loc with
        | Reg.Reg _ -> Int.Map.update acc cls ~f:increment
        | _ -> acc)
      registers
      Int.Map.empty
  in
  Int.Map.mapi live_regs ~f:(fun ~key ~data -> Proc.num_available_registers.(key) - data)

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

module Inst_id = struct
  module T = struct
    type t = int * int [@@deriving sexp, compare, equal]
  end

  include T
  module Map = Map.Make(T)
  module Set = Set.Make(T)
end

module Spill = struct
  (* Wrapper around integers and classes which identifies a spill slot *)
  module T = struct
    type t = int * int [@@deriving sexp, compare]
  end

  include T
  module Set = Set.Make(T)
  module Map = Map.Make(T)

  let all_spills cfg =
    (* Set of all used spill slots in a program. *)
    List.fold_left (Cfg.blocks cfg) ~init:Set.empty ~f:(fun spills block ->
      List.fold_left (BB.body block) ~init:spills ~f:(fun spills inst ->
        match inst.desc with
        | Cfg.Op Cfg.Spill ->
          Array.fold inst.Cfg.res ~init:spills ~f:(fun spills reg ->
            match reg.Reg.loc with
            | Reg.Stack (Local n) -> Set.add spills (Proc.register_class reg, n)
            | _ -> spills)
        | _ -> spills))

  let all_reloads cfg (cls, slot) =
    (* Set of all reloads of a specific slot in a program. *)
    List.fold_left (Cfg.blocks cfg) ~init:Inst_id.Set.empty ~f:(fun reloads block ->
      List.fold_left (BB.body block) ~init:reloads ~f:(fun reloads inst ->
        match inst.desc with
        | Cfg.Op (Cfg.Reload | Cfg.Move) ->
          Array.fold inst.Cfg.arg ~init:reloads ~f:(fun reloads reg ->
            match reg.Reg.loc with
            | Reg.Stack (Reg.Local s) when Proc.register_class reg = cls && s = slot->
              Inst_id.Set.add reloads (BB.start block, inst.Cfg.id)
            | _ -> reloads)
        | _ -> reloads))
end

module Spill_use_class = struct
  module Reload_class = struct
    type t =
      { path: Path_use.t
      ; pressure: Path_use.t
      }
      [@@deriving sexp, compare, equal]

    let lub a b =
      { path = Path_use.lub a.path b.path
      ; pressure = Path_use.lub a.pressure b.pressure
      }
  end

  module Use_class = struct
    type t
      = Path_use.t * Reload_class.t Inst_id.Map.t
      [@@deriving sexp, compare, equal]
  end

  type t = Use_class.t Spill.Map.t [@@deriving sexp]

  let equal = Spill.Map.equal Use_class.equal

  let lub a b =
    Spill.Map.merge a b ~f:(fun ~key:_ v ->
      match v with
      | `Both ((fa, ra), (fb, rb)) ->
        let f = Path_use.lub fa fb in
        let r =
          Inst_id.Map.merge ra rb ~f:(fun ~key:_ v ->
            match v with
            | `Both(rca, rcb) ->
              Some (Reload_class.lub rca rcb)
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
          let open Reload_class in
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

module Spill_use_classify_problem = struct
  module K = struct
    module S = Spill_use_class

    type t =
      { kills: Spill.Set.t
      ; gens: S.Reload_class.t Inst_id.Map.t Spill.Map.t
      ; pressure: bool
      ; freq: Frequency.t
      }

    let f s { kills; gens; pressure; freq } =
      let not_killed =
        Spill.Map.mapi s ~f:(fun ~key ~data ->
          let open Spill_use_class.Reload_class in
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
                    { path = update_never freq p.path
                    ; pressure = if pressure then Path_use.Always freq else p.pressure
                    }
                  in
                  spill_use)
              in
              (update_never freq use_freq, reloads'))
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
            let open S.Reload_class in
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

  type t =
    { cfg: Cfg.t
    ; cfg_info: Cfg_info.t option
    }

  let cfg { cfg; _ } = cfg

  let init { cfg; _ } block =
    let bb = Cfg.get_block_exn cfg block in
    if BB.is_exit bb then
      (Spill_use_class.all_unused_spills cfg, Spill.Map.empty)
    else
      (Spill.Map.empty, Spill.Map.empty)

  let kg { cfg; cfg_info } inst =
    let block = Cfg_inst_id.parent inst in
    let bb = Cfg.get_block_exn cfg block in
    let freq = Frequency.create cfg_info block in
    let open K in
    let nop pressure =
      { kills = Spill.Set.empty
      ; gens = Spill.Map.empty
      ; pressure
      ; freq
      }
    in
    match inst with
    | Cfg_inst_id.Term _ ->
      let term = BB.terminator bb in
      nop (has_pressure term (Cfg.destroyed_at_terminator term.desc))
    | Cfg_inst_id.Inst (_, n) ->
      let i = List.nth_exn (BB.body bb) n in
      let pressure = has_pressure i (Cfg.destroyed_at_instruction i.desc) in
      match i.Cfg.desc with
      | Cfg.Op Cfg.Spill ->
        (match i.Cfg.res with
        | [| { loc = Reg.Stack (Reg.Local s); _} as reg |] ->
          { (nop pressure) with kills = Spill.Set.singleton (Proc.register_class reg, s) }
        | _ -> failwith "invalid spill")
      | Cfg.Op (Cfg.Reload | Cfg.Move) ->
        (match i.Cfg.arg with
        | [| { loc = Reg.Stack (Reg.Local s); _} as reg |] ->
          let open Spill_use_class in
          let spill_use =
              { Reload_class.path = Path_use.Always freq
              ; pressure =
                if pressure
                  then Path_use.Always freq
                  else Path_use.Never freq
              }
          in
          let id = block, i.Cfg.id in
          let gens =
            Spill.Map.singleton
              (Proc.register_class reg, s)
              (Inst_id.Map.singleton id spill_use)
          in
          { (nop pressure) with gens }
        | [| _ |] -> nop pressure
        | _ -> failwith "invalid reload")
      | _ ->
        nop pressure
end

module Spill_use_classify = Analysis.Make_backward_cfg_solver(Spill_use_classify_problem)

module Spill_to_reload = struct
  module Reload = struct
    type t =
      { path: Path_use.t
      ; pressure: Path_use.t
      ; reg_use: Path_use.t
      }
      [@@deriving sexp]
  end

  module Spill = struct
    type t =
      { all_uses: Path_use.t
      ; reloads: Reload.t Inst_id.Map.t
      }
      [@@deriving sexp]
  end

  type t
    = Spill.t Inst_id.Map.t
    [@@deriving sexp]
end

(*
let get_block = function
  | Cfg_inst_id.Term block -> block
  | Cfg_inst_id.Inst(block, _) -> block

let get_inst_id cfg = function
  | Cfg_inst_id.Term block ->
    let bb = Cfg.get_block_exn cfg block in
    (BB.terminator bb).id
  | Cfg_inst_id.Inst(block, n) ->
    let bb = Cfg.get_block_exn cfg block in
    let i = List.nth_exn (BB.body bb) n in
    i.Cfg.id
*)

let score cl ~cfg_info =
  let cfg = CL.cfg cl in
  let reg_uses = Reg_use_classify.solve { cfg; cfg_info } in
  let spill_uses = Spill_use_classify.solve { cfg; cfg_info } in

  let reloads_of_spill reload_uses =
    Inst_id.Map.filter_mapi reload_uses ~f:(fun ~key ~data ->
      let block, reload_id = key in
      let { Spill_use_class.Reload_class.path; pressure } = data in
      match path with
      | Path_use.Never _ -> None
      | _ ->
        let reload_bb = Cfg.get_block_exn cfg block in
        let cfg_inst_idx, reload =
          List.find_mapi_exn (BB.body reload_bb) ~f:(fun idx i ->
            if i.Cfg.id = reload_id then Some (idx, i) else None)
        in
        let cfg_reload_id = Cfg_inst_id.Inst(block, cfg_inst_idx) in
        let all_reload_uses_at, _ = Cfg_inst_id.Map.find cfg_reload_id reg_uses in
        let reload_reg =
          match reload.Cfg.desc, reload.Cfg.res with
          | Cfg.Op (Cfg.Reload | Cfg.Move), [| { loc = Reg.Reg r; _ } |] -> r
          | _ -> failwith "invalid reload"
        in
        let reg_use = Register.Map.find_exn all_reload_uses_at reload_reg in
      Some (Spill_to_reload.Reload.({ path; pressure; reg_use })))
  in

  let spill_reloads =
    List.fold_left (Cfg.blocks cfg) ~init:Inst_id.Map.empty ~f:(fun acc block ->
      List.foldi (BB.body block) ~init:acc ~f:(fun idx acc i ->
        Option.value ~default:acc (
          let open Option.Let_syntax in
          let%bind s =
            match i.Cfg.desc, i.Cfg.res with
            | Cfg.Op Cfg.Spill, [| { loc = Reg.Stack (Reg.Local s); _} as reg |] ->
              Some (Proc.register_class reg, s)
            | _ -> None
          in
          let cfg_spill_id = Cfg_inst_id.Inst (BB.start block, idx) in
          let%bind all_spill_uses_at, _ =
            Cfg_inst_id.Map.find_opt cfg_spill_id spill_uses
          in
          let%map all_uses, reload_uses =
            Spill.Map.find all_spill_uses_at s
          in
          let reloads = reloads_of_spill reload_uses in
          let key = BB.start block, i.Cfg.id in
          let data = { Spill_to_reload.Spill.all_uses; reloads } in
          Inst_id.Map.set acc ~key ~data)))
  in
  (*
  Cfg_inst_id.Map.iter
    (fun id (use_in, use_out) ->
      Printf.printf "> %d %d\n" (get_block id) (get_inst_id cfg id);
      print_s [%message (use_in : Register_use_class.t)];
      print_s [%message (use_out : Register_use_class.t)]
    )
    reg_uses;
  Cfg_inst_id.Map.iter
    (fun id (use_in, use_out) ->
      Printf.printf "> %d %d\n" (get_block id) (get_inst_id cfg id);
      print_s [%message (use_in : Spill_use_class.t)];
      print_s [%message (use_out : Spill_use_class.t)]
    )
    spill_uses;
  *)
  print_endline (Cfg.fun_name cfg);
  (*Option.iter cfg_info ~f:(fun t -> Cfg_info.dump_dot t "");*)
  CL.save_as_dot cl "";
  print_s [%message (spill_reloads : Spill_to_reload.t)];
  Printf.printf "\n\n"

let score files ~fdo_profile =
  let profile = Option.map fdo_profile ~f:Aggregated_decoded_profile.read_bin in
  List.iter files ~f:(fun file ->
    let open Linear_format in
    let ui, _ = restore file in
    List.iter ui.items ~f:(fun item ->
      match item with
      | Data _ -> ()
      | Func f ->
        let cl = CL.of_linear f ~preserve_orig_labels:false in
        let name = Cfg.fun_name (CL.cfg cl) in
        let cfg_info = Option.bind profile ~f:(fun p ->
          Linearid_profile.create_cfg_info p name cl ~alternatives:[])
        in
        if List.length (Cfg.blocks (CL.cfg cl)) > 16 then ()
        else score cl ~cfg_info))
