open Core
open Core.Poly
module C = Ocamlcfg.Cfg
module CL = Ocamlcfg.Cfg_with_layout

let verbose = ref false

let validate = ref false

type reorder_algo =
  | Identity
  | Random of Random.State.t
  | Profile of Aggregated_decoded_profile.t

let print_list msg l = Report.log (sprintf !"%s: %{sexp:int list}\n" msg l)

(* All dead blocks should have been eliminated by earlier compiler stages,
   but the functionality for doing it is not fully-implemented yet, and some
   dead blocks remain. We can easily identify and eliminate them using CFG
   representation. However, it makes otherwise identical linear IRs to fail
   comparisons when we use the algorithm. *)
let check cl new_cfg_layout =
  let orig_cfg_layout = CL.layout cl in
  let cfg = CL.cfg cl in
  if not (new_cfg_layout = orig_cfg_layout) then (
    Report.log (sprintf "Reordered %s\n" (C.fun_name cfg));
    print_list "orig" orig_cfg_layout;
    print_list "new " new_cfg_layout;
    if !validate then
      (* Make sure the new layout is just a permutation. CR-soon gyorsh: do
         we need to handle block duplication here? *)
      let compare = Int.compare in
      let orig_len = List.length orig_cfg_layout in
      if CL.preserve_orig_labels cl then (
        assert (List.length new_cfg_layout = orig_len);
        assert (List.hd new_cfg_layout = List.hd orig_cfg_layout);
        assert (
          List.sort new_cfg_layout ~compare
          = List.sort orig_cfg_layout ~compare ) )
      else (
        assert (List.length new_cfg_layout <= orig_len);
        assert (List.hd new_cfg_layout = List.hd orig_cfg_layout) ) )

let reorder_random cl ~random_state =
  (* Ensure entry exit invariants *)
  let original_layout = CL.layout cl in
  let new_layout =
    List.hd_exn original_layout
    :: List.permute ~random_state (List.tl_exn original_layout)
  in
  check cl new_layout;
  CL.set_layout cl new_layout;
  cl

(* Basic block layout using clustering algorihtm. *)
let reorder_opt cfg_info cl =
  let orig_cfg_layout = CL.layout cl in
  let new_cfg_layout =
    Profile.record_call ~accumulate:true "optimize_layout"
      (fun () -> Clusters.optimize_layout orig_cfg_layout cfg_info) in
  check cl new_cfg_layout;
  CL.set_layout cl new_cfg_layout;
  cl

(* Compute cfg execounts even if reordering is not enabled. They can be saved
   to a file for later use. *)
(* Check if function-specific profile has already been computed. it doesn't
   make sense in the current setup, because all parallel jenga processes will
   be accessing the same file for write, but it sould work when we change the
   way profiles are stored to allow faster parallel access. *)
(* Could write to file intermediate per-function profiles. It would save
   recomping the counters but that's not long and there would be many files. *)
let reorder_profile cl linearid_profile =
  let name = C.fun_name (CL.cfg cl) in
  let cfg_info =
    Profile.record_call ~accumulate:true "cfg_info"
    (fun () -> Aggregated_decoded_profile.add linearid_profile name cl) in
  match cfg_info with
  | None -> cl
  | Some cfg_info -> reorder_opt cfg_info cl

let apply ~algo cfg =
  match algo with
  | Identity ->
      if !verbose then (
        printf "Don't reorder.\n";
        print_list "layout" (CL.layout cfg) );
      cfg
  | Random random_state -> reorder_random cfg ~random_state
  | Profile linearid_profile -> reorder_profile cfg linearid_profile

let hot_functions ~linearid_profile ~reorder_functions =
  (* Create linker script fragment with hot functions *)
  let open Config_reorder.Reorder_functions in
  match reorder_functions with
  | No -> []
  | Execounts -> Aggregated_decoded_profile.top_functions linearid_profile
  | Hot_clusters ->
      (* Do we ever need the cfg to decide on function order? *)
      failwith "Not implemented"
