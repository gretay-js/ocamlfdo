open Core

module Cfg = Ocamlcfg.Cfg
module BB = Cfg.Basic_block

module T = struct
  type t = int * int [@@deriving sexp, compare, equal]
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

let all_reloads cfg slot =
  (* Set of all reloads of a specific slot in a program. *)
  List.fold_left (Cfg.blocks cfg) ~init:Inst_id.Set.empty ~f:(fun reloads block ->
    List.fold_left (BB.body block) ~init:reloads ~f:(fun reloads inst ->
      Array.fold inst.Cfg.arg ~init:reloads ~f:(fun reloads reg ->
        match reg.Reg.loc with
        | Reg.Stack (Reg.Local s) when equal (Proc.register_class reg, s) slot ->
          Inst_id.Set.add reloads (BB.start block, inst.Cfg.id)
        | _ ->
          reloads)))
