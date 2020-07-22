open Core
open Ocamlcfg.Analysis

module Label = Ocamlcfg.Label
module Cfg = Ocamlcfg.Cfg
module BB = Cfg.Basic_block
module CL = Ocamlcfg.Cfg_with_layout
module AD = Aggregated_decoded_profile

type spill_reload_path =
  { start_bb: Label.t
  ; start_inst: int
  ; end_bb: Label.t
  ; end_inst: int
  ; stack_slot: int
  ; register_class: int
  }

let has_unconditional_spill path =
  let open Cfg in
  List.exists path ~f:(function
    | `Basic inst ->
      match inst.desc with
      | Call _ -> true
      | _ -> false)

let mkcount () = Array.create ~len:Proc.num_register_classes 0

let increment arr i = Array.set arr i (Array.get arr i + 1)

let fold_by_class arrays ~f =
  let agg_by_class = mkcount () in
  List.iter arrays ~f:(fun regs ->
    for cls = 0 to Proc.num_register_classes - 1 do
      let m = Array.get agg_by_class cls in
      let n = Array.get regs cls in
      Array.set agg_by_class cls (f m n)
    done);
  agg_by_class

let max_register_pressure path =
  (* Find the max register pressure along a given path. *)
  path
    |> List.map ~f:(function
      | `Basic inst ->
        let reg_count = mkcount () in
        Reg.Set.iter (fun reg ->
          increment reg_count (Proc.register_class reg)) inst.Cfg.live;
        reg_count)
    |> fold_by_class ~f:Int.max

let max_used_regs path =
  (* Find the maximal number of registers used along a given path. *)
  path
    |> List.map ~f:(function
      | `Basic inst ->
        let reg_count = mkcount () in
        Reg.Set.iter (fun reg ->
          match reg.Reg.loc with
          | Reg _ -> increment reg_count (Proc.register_class reg)
          | _ -> ()) inst.Cfg.live;
        reg_count)
    |> fold_by_class ~f:Int.max

let analyse_path _cfg spill_reload path =
  let pressure = max_register_pressure path in
  let regs = max_used_regs path in
  let out_of_regs = Array.existsi regs ~f:(fun i cnt ->
    cnt >= Array.get Proc.num_available_registers i)
  in
  let high_pressure = Array.existsi pressure ~f:(fun i cnt ->
    cnt >= int_of_float (float_of_int (Array.get Proc.num_available_registers i) *. 1.2))
  in
  if has_unconditional_spill path || (out_of_regs && high_pressure) then
    (* If the path has an OCaml call or has high register pressure, spills are unavoidable *)
    ()
  else begin
    Printf.printf "PATH %d %d %d %d: \n" spill_reload.start_bb spill_reload.end_bb spill_reload.start_inst spill_reload.end_inst;
    List.iter path ~f:(function
      | `Basic inst -> Printf.printf "  %a\n" Cfg.print_basic inst);
    Printf.printf "\nPRESSURE: ";
    Array.iter pressure ~f:(Printf.printf " %d");
    Printf.printf "\nREGS: ";
    Array.iter regs ~f:(Printf.printf " %d");
    Printf.printf "\n\n";
  end

let enumerate_paths cfg first last ~f =
  let succ node =
    Cfg.all_successor_labels cfg (Cfg.get_block_exn cfg node)
  in
  let pred node =
    Cfg.predecessor_labels (Cfg.get_block_exn cfg node)
  in
  let rec find_reachable next visited node =
    if Label.Set.mem node visited then visited
    else List.fold_left (next node) ~init:(Label.Set.add node visited) ~f:(find_reachable next)
  in
  let first_reachable = find_reachable succ Label.Set.empty first in
  let last_reachable = find_reachable pred Label.Set.empty last in
  let in_path = Label.Set.inter first_reachable last_reachable in
  let rec dfs path visited node =
    if not (Label.Set.mem node in_path) then ()
    else if Label.Set.mem node visited then ()
    else if node = last then f (node :: path)
    else
      let path' = node :: path in
      let visited' = Label.Set.add node visited in
      List.iter (succ node) ~f:(dfs path' visited')
  in
  dfs [] Label.Set.empty first

let path_to_instructions cfg { start_bb; start_inst; end_bb; end_inst; _ } path =
  (* Find all the instructions between the spill and the reload *)
  path
  |> List.map ~f:(fun block ->
    let body =
      BB.body (Cfg.get_block_exn cfg block)
      |> List.map ~f:(fun inst -> `Basic inst)
    in
    let id = function
      | `Basic i -> i.Cfg.id
    in
    if block = start_bb && block = end_bb then
      let before, after =
        List.split_while body ~f:(fun i -> id i <> start_inst)
      in
      if List.exists before ~f:(fun i -> id i = end_inst) then
        List.append
          (List.tl_exn after)
          (List.take_while before ~f:(fun i -> id i <> end_inst))
      else
        List.take_while after ~f:(fun i -> id i <> end_inst)
    else if block = start_bb then
      body
      |> List.drop_while ~f:(fun i -> id i <> start_inst)
      |> List.tl_exn
    else if block = end_bb then
      List.take_while body ~f:(fun i -> id i <> end_inst)
    else body)
  |> List.concat

let score cl ~cfg_info:_ =
  let cfg = CL.cfg cl in

  let reach = ReachingSpills.solve cfg in
  let spill_reload =
    List.fold_left (Cfg.blocks cfg) ~init:[] ~f:(fun srs block ->
      (* Match reloads to spills reaching them *)
      BB.body block
        |> List.filter_map ~f:(fun inst ->
          match inst.desc with
          | Op Reload ->
            (match inst.arg with
            | [| { Reg.loc = Reg.Stack (Reg.Local n); _ } |] ->
              (match inst.res with
              | [| reg |] ->
                Some (n, inst.id, Proc.register_class reg)
              | _ -> failwith "invalid reload")
            | _ -> None)
          | _ -> None)
        |> List.fold_left ~init:srs ~f:(fun srs (stack_slot, end_inst, register_class) ->
          match Caml.Hashtbl.find reach (BB.start block, end_inst) with
          | spills ->
            Spill.Set.fold
              (fun spill srs ->
                if spill.stack_slot <> stack_slot then srs
                else
                  { start_bb = spill.block
                  ; start_inst = spill.inst
                  ; end_bb = BB.start block
                  ; end_inst
                  ; stack_slot
                  ; register_class
                  } :: srs) spills srs
          | exception Caml.Not_found -> srs))
  in
  List.iter spill_reload ~f:(fun spill_reload ->
    let { start_bb; end_bb; _ } = spill_reload in
    enumerate_paths cfg start_bb end_bb ~f:(fun path ->
        (* Iterate over spill-reload pairs and identify all paths between them. *)
        (* Filter out paths to the end block which do no contain the start. *)
        let after_start =
          path
          |> List.rev
          |> List.drop_while ~f:(fun elem -> elem <> start_bb)
        in
        match after_start with
        | [] -> ()
        | path ->
           analyse_path cfg spill_reload (path_to_instructions cfg spill_reload path)))

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
