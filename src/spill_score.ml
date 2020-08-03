open Core

module Label = Ocamlcfg.Label
module Analysis = Ocamlcfg.Analysis
module CL = Ocamlcfg.Cfg_with_layout
module CP = Ocamlcfg.Passes
module Cfg = Ocamlcfg.Cfg
module BB = Cfg.Basic_block
module AD = Aggregated_decoded_profile
module Cfg_inst_id = Analysis.Inst_id

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

let reloads_of_spill cfg slot ~reg_uses ~reloads =
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
        let cfg_reload_id, res =
          if term.Cfg.id = reload_id
            then Cfg_inst_id.Term block , term.Cfg.res
            else
              List.find_mapi_exn (BB.body reload_bb) ~f:(fun idx i ->
                if i.Cfg.id = reload_id
                  then Some (Cfg_inst_id.Inst(block, idx), i.Cfg.res)
                  else None)
        in
        let all_reload_uses_at, _ = Cfg_inst_id.Map.find cfg_reload_id reg_uses in
        let reg_use =
          res
            |> Array.to_list
            |> List.filter_map ~f:(function
              | { loc = Reg.Reg r; _ } ->
                Some (Register.Map_with_default.find all_reload_uses_at r)
              | _ -> None)
            |> List.fold_left ~init:Path_use.never ~f:Path_use.max
        in
        Some (key, Spill_to_reload.Reload.({ path; pressure; reg_use })))
    |> Inst_id.Map.of_alist_exn

let score cl ~cfg_info =
  let cfg = CL.cfg cl in

  (match cfg_info with
  | Some info -> Cfg_info.dump_dot info ""
  | None -> CL.save_as_dot cl "");
  let cfg_info = None in
  Printf.printf "%s %d\n" (Cfg.fun_name cfg) (List.length (Cfg.blocks cfg));
  let reg_uses = Register_use.Solver.solve cfg cfg_info in
  print_endline "X";
  let spill_uses = Spill_use.Solver.solve cfg cfg_info in
  print_endline "Y";

  let spill_reloads =
    List.fold_left (Cfg.blocks cfg) ~init:Inst_id.Map.empty ~f:(fun acc block ->
      List.foldi (BB.body block) ~init:acc ~f:(fun idx acc i ->
        Array.fold i.Cfg.res ~init:acc ~f:(fun acc reg ->
          match reg with
          | { loc = Reg.Stack (Reg.Local s); _} ->
            let slot = Proc.register_class reg, s in
            let cfg_spill_id = Cfg_inst_id.Inst (BB.start block, idx) in
            (match Cfg_inst_id.Map.find_opt cfg_spill_id spill_uses with
            | Some (all_spill_uses_at, _) ->
              let { Spill_use.Class.Uses.all_uses; reloads } =
                Spill.Map_with_default.find all_spill_uses_at slot
              in
              let reloads = reloads_of_spill cfg slot ~reg_uses ~reloads in
              let key = BB.start block, i.Cfg.id in
              let data = { Spill_to_reload.Spill.all_uses; reloads } in
              Inst_id.Map.set acc ~key ~data
            | None -> acc)
          | _ -> acc)))
  in
  print_s [%message (spill_reloads : Spill_to_reload.t)];
  Printf.printf "\n\n"

let score files ~fdo_profile ~simplify_cfg =
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
        score cl ~cfg_info))
