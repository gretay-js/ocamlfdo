(**************************************************************************)
(*                                                                        *)
(*                                 OCamlFDO                               *)
(*                                                                        *)
(*                     Greta Yorsh, Jane Street Europe                    *)
(*                                                                        *)
(*   Copyright 2019 Jane Street Group LLC                                 *)
(*                                                                        *)
(*   All rights reserved.  This file is distributed under the terms of    *)
(*   the GNU Lesser General Public License version 2.1, with the          *)
(*   special exception on linking described in the file LICENSE.          *)
(*                                                                        *)
(**************************************************************************)
(* All dead blocks should have been eliminated by earlier compiler stages,
   but the functionality for doing it is not fully-implemented yet, and some
   dead blocks remain. We can easily identify and eliminate them using CFG
   representation. However, it makes otherwise identical linear IRs to fail
   comparisons when we use the algorithm to. *)
open Core

let verbose = false

type layout = int list String.Map.t

module Config = struct
  type reorder_basic_blocks = No | Opt

  type reorder_functions = No | Execounts | Hot_clusters

  type t =
    { gen_linearid_profile: string
    ; write_bolt_fdata: bool
    ; write_linker_script: bool
    ; reorder_basic_blocks: reorder_basic_blocks
    ; reorder_functions: reorder_functions }

  let default gen_linearid_profile =
    { gen_linearid_profile
    ; write_bolt_fdata= true
    ; write_linker_script= true
    ; reorder_functions= No
    ; reorder_basic_blocks= No }

  let linker_script_hot_extension = "linker-script-hot"

  let linker_script_filename t stage =
    sprintf "%s%s%s.%s" t.gen_linearid_profile
      (if String.is_empty stage then "" else ".")
      stage linker_script_hot_extension
end

type reorder_algo =
  | Identity
  | Random of Random.State.t
  | Linear of layout
  | Cfg of layout
  | Profile of Profiles.Aggregated_decoded.t * Config.t

let validate cfg new_cfg_layout =
  let orig_cfg_layout = Cfg_builder.get_layout cfg in
  (* Make sure the new layout is just a permutation. CR gyorsh: do we need
     to handle block duplication here? *)
  if not (new_cfg_layout = orig_cfg_layout) then (
    let compare = Int.compare in
    let orig_len = List.length orig_cfg_layout in
    if Cfg_builder.preserve_orig_labels cfg then (
      assert (List.length new_cfg_layout = orig_len) ;
      assert (List.hd new_cfg_layout = List.hd orig_cfg_layout) ;
      assert (
        List.sort new_cfg_layout ~compare
        = List.sort orig_cfg_layout ~compare ) )
    else (
      assert (List.length new_cfg_layout <= orig_len) ;
      assert (List.hd new_cfg_layout = List.hd orig_cfg_layout) ) ;
    Report.log (sprintf "Reordered %s\n" (Cfg_builder.get_name cfg)) )

let reorder_random cfg ~random_state =
  (* Ensure entry exit invariants *)
  let original_layout = Cfg_builder.get_layout cfg in
  let new_layout =
    List.hd_exn original_layout
    :: List.permute ~random_state (List.tl_exn original_layout)
  in
  validate cfg new_layout ;
  Cfg_builder.set_layout cfg new_layout

let print_list msg l =
  if verbose then Printf.printf !"%s: %{sexp:int list}\n" msg l

exception KeyAlreadyPresent of int * int

let reorder_layout cfg ~layout =
  let fun_name = Cfg_builder.get_name cfg in
  try
    match String.Map.find layout fun_name with
    | None -> cfg
    | Some sorted_fun_layout ->
        if verbose then Cfg_builder.print stdout cfg ;
        let orig_cfg_layout = Cfg_builder.get_layout cfg in
        print_list "orig" orig_cfg_layout ;
        print_list "sorted_fun_layout" sorted_fun_layout ;
        (* Convert linear_ids to labels *)
        let partial_cfg_layout =
          List.map sorted_fun_layout ~f:(fun id ->
              if verbose then Printf.printf "%d\n" id ;
              match Cfg_builder.id_to_label cfg id with
              | None ->
                  failwithf "Cannot find label for linear id %d in %s\n" id
                    fun_name ()
              | Some label -> label )
          |> List.remove_consecutive_duplicates ~equal:Int.equal
        in
        print_list "partial:" partial_cfg_layout ;
        let partial_cfg_map =
          List.foldi partial_cfg_layout ~init:Int.Map.empty
            ~f:(fun i m label ->
              match Map.add m ~key:label ~data:i with
              | `Duplicate -> raise (KeyAlreadyPresent (label, i))
              | `Ok m -> m )
        in
        (* Create new layout that extends partial layout with missing labels
           in the original order. To do this naively: walk over the original
           layout and check for each pair of consecutive labels (prev, curr)
           if curr does not appear in partial layout, find prev in partial
           layout and insert curr after it. We break the original layout
           into sub-layouts such that only the first label in each sublayout
           appears in the partial layout, and then reorder based on the
           partial layout. *)
        let new_cfg_layout =
          List.group orig_cfg_layout ~break:(fun _prev cur ->
              Map.mem partial_cfg_map cur )
          |> List.sort ~compare:(fun l1 l2 ->
                 let h1 = List.hd_exn l1 in
                 let h2 = List.hd_exn l2 in
                 let index1 = Map.find_exn partial_cfg_map h1 in
                 let index2 = Map.find_exn partial_cfg_map h2 in
                 Int.compare index1 index2 )
          |> List.concat
        in
        print_list "new:" new_cfg_layout ;
        validate cfg new_cfg_layout ;
        Cfg_builder.set_layout cfg new_cfg_layout
  with KeyAlreadyPresent (id, pos) ->
    Report.log
      (sprintf "Ignoring %s: cannot add linear_id %d at position %d\n"
         fun_name id pos) ;
    cfg

let reorder_rel_layout cfg ~layout =
  let fun_name = Cfg_builder.get_name cfg in
  match String.Map.find layout fun_name with
  | None -> cfg
  | Some new_cfg_layout ->
      if verbose then Cfg_builder.print stdout cfg ;
      let orig_cfg_layout = Cfg_builder.get_layout cfg in
      print_list "orig" orig_cfg_layout ;
      print_list "new" new_cfg_layout ;
      validate cfg new_cfg_layout ;
      Cfg_builder.set_layout cfg new_cfg_layout

(* Basic block layout using clustering algorihtm. *)
let reorder_opt execounts cfg =
  let orig_cfg_layout = Cfg_builder.get_layout cfg in
  print_list "orig" orig_cfg_layout ;
  let new_cfg_layout = Clusters.optimize_layout orig_cfg_layout execounts in
  print_list "new" new_cfg_layout ;
  validate cfg new_cfg_layout ;
  Cfg_builder.set_layout cfg new_cfg_layout

let write_profile linearid_profile config =
  let open Config in
  if config.write_linker_script then (
    let linker_script_hot = Config.linker_script_filename config "" in
    if verbose then
      printf "Writing linker script hot to %s\n" linker_script_hot ;
    match config.reorder_functions with
    | No ->
        if verbose then
          printf
            "Reorder functions is not enabled.Cannot output linker script.\n"
    | Execounts ->
        Profiles.Aggregated_decoded.write_top_functions linearid_profile
          linker_script_hot
    | Hot_clusters -> failwith "Not implemented" )

let reorder_profile cfg linearid_profile config =
  let open Config in
  let name = Cfg_builder.get_name cfg in
  (* Compute cfg counts even if reordering is not enabled. The are stored in
     the linearid_profile for later use. *)
  let execounts =
    Profiles.Aggregated_decoded.compute_cfg_execounts linearid_profile name
      cfg
  in
  match config.reorder_basic_blocks with
  | No -> cfg
  | Opt -> (
    match execounts with
    | None -> cfg
    | Some execounts -> reorder_opt execounts cfg )

let reorder ~algo cfg =
  match algo with
  | Identity -> cfg
  | Random random_state -> reorder_random cfg ~random_state
  | Cfg layout -> reorder_rel_layout cfg ~layout
  | Linear layout -> reorder_layout cfg ~layout
  | Profile (linearid_profile, config) ->
      reorder_profile cfg linearid_profile config

let finish_profile linearid_profile config =
  (* Call write_profile here after all cfgs are processed. It will only be
     needed after we implement reorder that depends on the cfg.*)
  write_profile linearid_profile config ;
  ()

let finish = function
  | Profile (linearid, options) -> finish_profile linearid options
  | _ -> ()
