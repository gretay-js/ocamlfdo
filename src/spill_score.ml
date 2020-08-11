open Core

module Label = Ocamlcfg.Label
module Analysis = Ocamlcfg.Analysis
module CL = Ocamlcfg.Cfg_with_layout
module CP = Ocamlcfg.Passes
module Cfg = Ocamlcfg.Cfg
module BB = Cfg.Basic_block
module AD = Aggregated_decoded_profile
module Cfg_inst_id = Ocamlcfg.Inst_id

module Spill_to_reload = struct
  module Reload = struct
    type t =
      { freq: Frequency.t;
        path: Path_use.t;
        pressure: Path_use.t;
        reg_use: Path_use.t;
      }
      [@@deriving sexp]
  end

  module Spill = struct
    type t =
      { freq: Frequency.t;
        all_uses: Path_use.t;
        reloads: Reload.t Inst_id.Map.t;
      }
      [@@deriving sexp]

    let is_unavoidable { reloads; _ } =
      (* A spill is unavoidable if there is pressure to all reloads. *)
      Inst_id.Map.for_all reloads ~f:(fun { pressure; _ } -> Path_use.is_always pressure)
  end

  type t
    = Spill.t Inst_id.Map.t
    [@@deriving sexp]

  let is_empty = Inst_id.Map.is_empty
end

let inst_reloads arg slot =
  Array.fold arg ~init:false ~f:(fun reloads reg ->
    match reg.Reg.loc with
    | Reg.Stack (Reg.Local s) when Spill.equal (Proc.register_class reg, s) slot ->
      true
    | _ ->
      reloads)

let all_reloads cfg slot =
  (* Set of all reloads of a specific slot in a program. *)
  List.fold_left (Cfg.blocks cfg) ~init:Inst_id.Set.empty ~f:(fun reloads block ->
    let start = BB.start block in
    let term = BB.terminator block in
    let reloads =
      if inst_reloads term.Cfg.arg slot then
        Inst_id.Set.add reloads (start, term.Cfg.id)
      else
        reloads
    in
    List.fold_left (BB.body block) ~init:reloads ~f:(fun reloads inst ->
      if inst_reloads inst.Cfg.arg slot
        then Inst_id.Set.add reloads (BB.start block, inst.Cfg.id)
        else reloads))

let reloads_of_spill cfg slot ~cfg_info ~reg_uses ~reloads =
  all_reloads cfg slot
    |> Inst_id.Set.to_list
    |> List.filter_map ~f:(fun key ->
      let data = Inst_id.Map_with_default.find reloads key in
      let block, reload_id = key in
      let { Spill_use.Class.Use.path; pressure } = data in
      match path with
      | Path_use.Never _ -> None
      | _ ->
        let reload_bb = Cfg.get_block_exn cfg block in
        let term = BB.terminator reload_bb in
        (* Determine the ID and registers of the instruction and also identify
           instructions which use the value themselves, without having to pass it on
           to other users, such as terminators (through the control dependency) or
           stores and other instructions with side effects *)
        let cfg_reload_id, res, always_uses =
          if term.Cfg.id = reload_id
            then Cfg_inst_id.at_terminator block, term.Cfg.res, true
            else
              List.find_mapi_exn (BB.body reload_bb) ~f:(fun idx i ->
                if i.Cfg.id <> reload_id then None
                else
                  let open Cfg in
                  let always_uses =
                    match i.desc with
                    | Op (Store _) -> true
                    | _ -> false
                  in
                  Some (Cfg_inst_id.at_instruction block idx, i.Cfg.res, always_uses))
        in
        let all_reload_uses_at, _ = Cfg_inst_id.Map.find cfg_reload_id reg_uses in
        let reg_other_use =
          res
            |> Array.to_list
            |> List.filter_map ~f:(function
              | { loc = Reg.Reg r; _ } ->
                Some (Register.Map_with_default.find all_reload_uses_at r)
              | _ -> None)
            |> List.fold_left ~init:Path_use.never ~f:Path_use.max
        in
        let reg_use =
          if always_uses
            then Path_use.(max (Always (Frequency.create cfg_info block)) reg_other_use)
            else reg_other_use
        in
        let freq =  Frequency.create cfg_info block in
        Some (key, Spill_to_reload.Reload.({ path; pressure; reg_use; freq })))
    |> Inst_id.Map.of_alist_exn

let score cl ~cfg_info ~score_all =
  let cfg = CL.cfg cl in
  (* Run the data flow analyses to find register and spill slot users. *)
  let reg_uses = Register_use.Solver.solve cfg cfg_info in
  let spill_uses = Spill_use.Solver.solve cfg cfg_info in
  (* Map all reloads to the spills they read from. *)
  let spills =
    List.fold_left (Cfg.blocks cfg) ~init:Inst_id.Map.empty ~f:(fun acc block ->
      let start = BB.start block in
      let freq = Frequency.create cfg_info start in
      List.foldi (BB.body block) ~init:acc ~f:(fun idx acc i ->
        Array.fold i.Cfg.res ~init:acc ~f:(fun acc reg ->
          match reg with
          | { loc = Reg.Stack (Reg.Local s); _} ->
            let slot = Proc.register_class reg, s in
            let cfg_spill_id = Cfg_inst_id.at_instruction start idx in
            (match Cfg_inst_id.Map.find_opt cfg_spill_id spill_uses with
            | Some (all_spill_uses_at, _) ->
              let { Spill_use.Class.Uses.all_uses; reloads } =
                Spill.Map_with_default.find all_spill_uses_at slot
              in
              let reloads = reloads_of_spill cfg slot ~cfg_info ~reg_uses ~reloads in
              let key = start, i.Cfg.id in
              let open Spill_to_reload in
              let data = { Spill.freq; all_uses; reloads } in
              if Spill.is_unavoidable data || (not score_all && Int64.(freq < of_int 5))
                then acc
                else Inst_id.Map.set acc ~key ~data
            | None -> acc)
          | _ -> acc)))
  in
  (* Print information about the spills *)
  if not (Spill_to_reload.is_empty spills) then begin
    Printf.printf "%s %d\n" (Cfg.fun_name cfg) (List.length (Cfg.blocks cfg));
    Inst_id.Map.iteri spills ~f:(fun ~key:spill_id ~data:spill ->
      let open Spill_to_reload in
      Inst_id.Map.iteri spill.reloads ~f:(fun ~key:reload_id ~data:reload ->
        if Path_use.is_less_frequent reload.pressure then
          print_s [%message
            (spill_id: Inst_id.t)
            (reload_id : Inst_id.t)
            (reload : Reload.t)])
    );
    Printf.printf "\n\n"
  end

let score files ~fdo_profile ~simplify_cfg ~score_all =
  let profile = Option.map fdo_profile ~f:Aggregated_decoded_profile.read_bin in
  List.iter files ~f:(fun file ->
    let open Linear_format in
    let ui, _ = restore file in
    List.iter ui.items ~f:(fun item ->
      match item with
      | Data _ -> ()
      | Func f ->
        let cl = CL.of_linear f ~preserve_orig_labels:(not simplify_cfg) in
        let name = Cfg.fun_name (CL.cfg cl) in
        if simplify_cfg then begin
          CL.eliminate_fallthrough_blocks cl;
          CP.simplify_terminators (CL.cfg cl);
        end;
        let cfg_info = Option.bind profile ~f:(fun p ->
          Linearid_profile.create_cfg_info p name cl ~alternatives:[])
        in
        if Option.is_none cfg_info && not score_all
          then ()
          else score cl ~cfg_info ~score_all))
