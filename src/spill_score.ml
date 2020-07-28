open Core

module Label = Ocamlcfg.Label
module Analysis = Ocamlcfg.Analysis
module CL = Ocamlcfg.Cfg_with_layout
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

let score cl ~cfg_info =
  let cfg = CL.cfg cl in
  let reg_uses = Register_use.Solver.solve (cfg, cfg_info) in
  let spill_uses = Spill_use.Solver.solve (cfg, cfg_info) in

  let reloads_of_spill reload_uses =
    Inst_id.Map.filter_mapi reload_uses ~f:(fun ~key ~data ->
      let block, reload_id = key in
      let { Spill_use.Class.Use.path; pressure } = data in
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
        let reg_use =
          reload.Cfg.res
            |> Array.to_list
            |> List.filter_map ~f:(function
              | { loc = Reg.Reg r; _ } ->
                Some (Register.Map.find_exn all_reload_uses_at r)
              | _ -> None)
            |> List.fold_left ~init:Path_use.never ~f:Path_use.max
        in
      Some (Spill_to_reload.Reload.({ path; pressure; reg_use })))
  in

  let spill_reloads =
    List.fold_left (Cfg.blocks cfg) ~init:Inst_id.Map.empty ~f:(fun acc block ->
      List.foldi (BB.body block) ~init:acc ~f:(fun idx acc i ->
        Array.fold i.Cfg.res ~init:acc ~f:(fun acc reg ->
          match reg with
          | { loc = Reg.Stack (Reg.Local s); _} ->
            Option.value ~default:acc (
              let open Option.Let_syntax in
              let s = Proc.register_class reg, s in
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
              Inst_id.Map.set acc ~key ~data)
          | _ -> acc)))
  in
  print_endline (Cfg.fun_name cfg);
  (match cfg_info with
  | Some info -> Cfg_info.dump_dot info ""
  | None -> CL.save_as_dot cl "");
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
        score cl ~cfg_info))
