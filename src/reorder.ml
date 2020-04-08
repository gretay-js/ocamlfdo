open Core
open Core.Poly
module C = Ocamlcfg.Cfg
module CL = Ocamlcfg.Cfg_with_layout
module AD = Aggregated_decoded_profile

let verbose = ref false

let validate = ref false

type reorder_algo =
  | Identity
  | Random of Random.State.t
  | Profile of AD.t

let print_list msg l = Report.logf !"%s: %{sexp:int list}" msg l

(* All dead blocks should have been eliminated by earlier compiler stages,
   but the functionality for doing it is not fully-implemented yet, and some
   dead blocks remain. We can easily identify and eliminate them using CFG
   representation. However, it makes otherwise identical linear IRs to fail
   comparisons when we use the algorithm. *)
let check cl new_cfg_layout =
  let orig_cfg_layout = CL.layout cl in
  let cfg = CL.cfg cl in
  if not (new_cfg_layout = orig_cfg_layout) then (
    Report.logf
      !"Reordered %s\norig: %{sexp: int list}\nnew : %{sexp: int list}\n"
      (C.fun_name cfg) orig_cfg_layout new_cfg_layout;
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
    Profile.record_call ~accumulate:true "optimize_layout" (fun () ->
        Clusters.optimize_layout orig_cfg_layout cfg_info)
  in
  check cl new_cfg_layout;
  CL.set_layout cl new_cfg_layout;
  if !verbose then Cfg_info.dump_dot cfg_info "opt";
  cl

let reorder_profile cl p ~alternatives =
  let name = C.fun_name (CL.cfg cl) in
  match Linearid_profile.create_cfg_info p name cl ~alternatives with
  | None -> cl
  | Some cfg_info -> reorder_opt cfg_info cl

let apply ~algo cl ~alternatives =
  match algo with
  | Identity ->
      if !verbose then print_list "Don't reorder layout" (CL.layout cl);
      cl
  | Random random_state -> reorder_random cl ~random_state
  | Profile p -> reorder_profile cl p ~alternatives

(* Cutoff *)

open Core
module AD = Aggregated_decoded_profile

let verbose = ref true

let get_percent ~total p =
  if p < 0 || p > 100 then
    Report.user_error
      "Cannot take %d percent of the hot functions. Percent must be between \
       0 and 100."
      p;
  total * p / 100

let take_top_percent profile p =
  let top = AD.sorted_functions profile in
  let total = List.length top in
  let n = Percent.apply p (Float.of_int total) |> Float.to_int in
  List.take top n

let take_top profile n =
  let top = AD.sorted_functions profile in
  List.take top n

let take_min_samples profile min_val =
  let top = AD.sorted_functions_with_counts profile in
  List.take_while top ~f:(fun (_, count) ->
      Execount.(compare count (of_int min_val)) >= 0)
  |> List.unzip |> fst

let take_top_percent_samples profile p =
  let top = AD.sorted_functions_with_counts profile in
  let total =
    List.fold top ~init:0L ~f:(fun acc (_, count) -> Execount.(acc + count))
  in
  let max = Percent.apply p (Float.of_int64 total) |> Float.to_int64 in
  let list, sum =
    List.fold top ~init:([], 0L) ~f:(fun (list, sum) (name, count) ->
        if Execount.compare sum max < 0 then
          (name :: list, Execount.(sum + count))
        else (list, sum))
  in
  if !verbose then
    printf
      "take_top_percent_samples %s of total %Ld = %Ld, found %Ld in top %d \
       functions\n"
      (Percent.to_string p) total max sum (List.length list);
  List.rev list

let top_functions profile ~cutoff =
  let open Config_reorder.Cutoff_functions in
  match cutoff with
  | Top_percent p -> take_top_percent profile p
  | Top_percent_samples p -> take_top_percent_samples profile p
  | Top n -> take_top profile n
  | Top_clusters n -> failwith "Not implemented"
  | Min_samples n -> take_min_samples profile n

(* Do we ever need the cfg to decide on function order? *)
let hot_functions profile ~reorder_functions ~cutoff =
  (* Create linker script fragment with hot functions *)
  let open Config_reorder.Reorder_functions in
  match reorder_functions with
  | No -> []
  | Execounts -> top_functions profile ~cutoff
  | Random -> List.permute (AD.all_functions profile)
  | Hot_clusters -> failwith "Not implemented"
