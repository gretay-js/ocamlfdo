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
open Ocamlcfg

let verbose = ref true

type layout = int list String.Map.t

type reorder_algo =
  | Identity
  | Random of Random.State.t
  | Linear of layout
  | Cfg of layout
  | Profile of Aggregated_decoded_profile.t * Config_reorder.t

let print_layout layout =
  List.iter layout ~f:(fun lbl -> printf " %d" lbl);
  printf "\n"

let validate cfg new_cfg_layout =
  let orig_cfg_layout = Cfg_builder.get_layout cfg in
  (* Make sure the new layout is just a permutation. CR gyorsh: do we need
     to handle block duplication here? *)
  if not (new_cfg_layout = orig_cfg_layout) then (
    let compare = Int.compare in
    let orig_len = List.length orig_cfg_layout in
    if Cfg_builder.preserve_orig_labels cfg then (
      assert (List.length new_cfg_layout = orig_len);
      assert (List.hd new_cfg_layout = List.hd orig_cfg_layout);
      assert (
        List.sort new_cfg_layout ~compare
        = List.sort orig_cfg_layout ~compare ) )
    else (
      assert (List.length new_cfg_layout <= orig_len);
      assert (List.hd new_cfg_layout = List.hd orig_cfg_layout) );
    Report.log (sprintf "Reordered %s\n" (Cfg_builder.get_name cfg));
    if !verbose then (
      printf "orig: ";
      print_layout orig_cfg_layout;
      printf "new:  ";
      print_layout new_cfg_layout ) )

let reorder_random cfg ~random_state =
  (* Ensure entry exit invariants *)
  let original_layout = Cfg_builder.get_layout cfg in
  let new_layout =
    List.hd_exn original_layout
    :: List.permute ~random_state (List.tl_exn original_layout)
  in
  validate cfg new_layout;
  Cfg_builder.set_layout cfg new_layout

let print_list msg l =
  if !verbose then printf !"%s: %{sexp:int list}\n" msg l

exception KeyAlreadyPresent of int * int

let reorder_layout cfg ~layout =
  let fun_name = Cfg_builder.get_name cfg in
  try
    match String.Map.find layout fun_name with
    | None -> cfg
    | Some sorted_fun_layout ->
        if !verbose then Cfg_builder.print stdout cfg;
        let orig_cfg_layout = Cfg_builder.get_layout cfg in
        print_list "orig" orig_cfg_layout;
        print_list "sorted_fun_layout" sorted_fun_layout;

        (* Convert linear_ids to labels *)
        let partial_cfg_layout =
          List.map sorted_fun_layout ~f:(fun id ->
              if !verbose then printf "%d\n" id;
              match Cfg_builder.id_to_label cfg id with
              | None ->
                  failwithf "Cannot find label for linear id %d in %s\n" id
                    fun_name ()
              | Some label -> label)
          |> List.remove_consecutive_duplicates ~equal:Int.equal
        in
        print_list "partial:" partial_cfg_layout;
        let partial_cfg_map =
          List.foldi partial_cfg_layout ~init:Int.Map.empty
            ~f:(fun i m label ->
              match Map.add m ~key:label ~data:i with
              | `Duplicate -> raise (KeyAlreadyPresent (label, i))
              | `Ok m -> m)
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
              Map.mem partial_cfg_map cur)
          |> List.sort ~compare:(fun l1 l2 ->
                 let h1 = List.hd_exn l1 in
                 let h2 = List.hd_exn l2 in
                 let index1 = Map.find_exn partial_cfg_map h1 in
                 let index2 = Map.find_exn partial_cfg_map h2 in
                 Int.compare index1 index2)
          |> List.concat
        in
        print_list "new:" new_cfg_layout;
        validate cfg new_cfg_layout;
        Cfg_builder.set_layout cfg new_cfg_layout
  with KeyAlreadyPresent (id, pos) ->
    Report.log
      (sprintf "Ignoring %s: cannot add linear_id %d at position %d\n"
         fun_name id pos);
    cfg

let reorder_rel_layout cfg ~layout =
  let fun_name = Cfg_builder.get_name cfg in
  match String.Map.find layout fun_name with
  | None -> cfg
  | Some new_cfg_layout ->
      if !verbose then Cfg_builder.print stdout cfg;
      let orig_cfg_layout = Cfg_builder.get_layout cfg in
      print_list "orig" orig_cfg_layout;
      print_list "new" new_cfg_layout;
      validate cfg new_cfg_layout;
      Cfg_builder.set_layout cfg new_cfg_layout

(* Basic block layout using clustering algorihtm. *)
let reorder_opt cfg_info cfg =
  let orig_cfg_layout = Cfg_builder.get_layout cfg in
  print_list "orig" orig_cfg_layout;
  let new_cfg_layout = Clusters.optimize_layout orig_cfg_layout cfg_info in
  print_list "new" new_cfg_layout;
  validate cfg new_cfg_layout;
  Cfg_builder.set_layout cfg new_cfg_layout

let write_profile _linearid_profile _config = ()

(* let write_profile linearid_profile config =
 *   let open Config_reorder in
 *   if config.write_bolt_fdata then (
 *     let filename = get_bolt_fdata_filename config "ft" in
 *     Bolt_profile.save_fallthrough linearid_profile locations ~filename;
 *     let filename = get_bolt_decoded_filename config "ft" in
 *     Decoded_bolt_profile.save_fallthrough linearid_profile ~filename );
 *   Aggregated_decoded_profile.write linearid_profile
 *     config.linearid_profile_filename *)

let reorder_profile cfg linearid_profile config =
  let name = Cfg_builder.get_name cfg in
  (* Compute cfg execounts even if reordering is not enabled. They can be
     saved to a file for later use. *)
  (* Check if function-specific profile has already been computed. it
     doesn't make sense in the current setup, because all parallel jenga
     processes will be accessing the same file for write, but it sould work
     when we change the way profiles are stored to allow faster parallel
     access. *)
  let cfg_info = Aggregated_decoded_profile.add linearid_profile name cfg in
  (* could write to file intermediate per-function profiles. It would save
     recomping the counters but that's not long and there would be many
     files. *)
  match config.Config_reorder.reorder_blocks with
  | No -> cfg
  | Opt -> (
      match cfg_info with
      | None -> cfg
      | Some cfg_info -> reorder_opt cfg_info cfg )
  | Random ->
      (* We check earlier whether there is a profile. *)
      assert false

let reorder ~algo cfg =
  match algo with
  | Identity ->
      if !verbose then (
        printf "Don't reorder. Current layout=";
        print_layout (Cfg_builder.get_layout cfg) );
      cfg
  | Random random_state -> reorder_random cfg ~random_state
  | Cfg layout -> reorder_rel_layout cfg ~layout
  | Linear layout -> reorder_layout cfg ~layout
  | Profile (linearid_profile, config) ->
      reorder_profile cfg linearid_profile config

let finish_profile linearid_profile config =
  (* Call write_profile here after all cfgs are processed. It will only be
     needed after we implement reorder that depends on the cfg.*)
  write_profile linearid_profile config;
  ()

let finish = function
  | Profile (linearid, config) -> finish_profile linearid config
  | _ -> ()
