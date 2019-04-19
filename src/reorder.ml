(* All dead blocks should have been eliminated by earlier compiler stages,
   but the functionality for doing it is not fully-implemented yet,
   and some dead blocks remain.
   We can easily identify and eliminate them using CFG representation.
   However, it makes otherwise identical linear IRs to fail comparisons
   when we use the algorithm to. *)
open Core

let verbose = ref true

type fun_layout = (int, int) Hashtbl.t
type layout = (string, fun_layout) Hashtbl.t

type reorder_algo =
  | Identity
  | Random
  | External of layout
  | CachePlus

let reorder_random cfg =
  (* Ensure entry exit invariants *)
  let original_layout = Cfg.get_layout cfg in
  let new_layout = (List.hd_exn original_layout)::
                   (List.permute (List.tl_exn original_layout))
  in
  Cfg.set_layout cfg new_layout

let print_list msg l =
  if !verbose then
    Printf.printf !"%s: %{sexp:int list}\n" msg l

let reorder_layout cfg layout =
  let fun_name = Cfg.get_name cfg in
  match Hashtbl.find layout fun_name with
  | None -> cfg
  | Some fun_layout -> begin
      (* get linear ids in the new order *)
      let sorted_fun_layout =
        List.sort (Hashtbl.to_alist fun_layout)
          ~compare:(fun (k1, _) (k2,_) -> Int.compare k1 k2)
        |> List.map ~f:(fun (_k, d) -> d)
      in
      let orig_cfg_layout = Cfg.get_layout cfg in
      print_list "orig" orig_cfg_layout;
      print_list "sorted_fun_layout" sorted_fun_layout;
      (* Convert linear_ids to labels *)
      let partial_cfg_layout =
        List.map sorted_fun_layout
          ~f:(fun id ->
            Printf.printf "%d\n" id;
            match Cfg.id_to_label cfg id with
            | None -> Misc.fatal_errorf
                        "Cannot find label for linear id %d in %s\n"
                        id fun_name
            | Some label -> label
          )
        |> List.remove_consecutive_duplicates ~equal:Int.equal
      in
      print_list "partial:" partial_cfg_layout;
      let partial_cfg_map =
        List.foldi partial_cfg_layout
          ~init:Int.Map.empty
          ~f:(fun i m label -> Map.add_exn m ~key:label ~data:i)
      in
      (* Create new layout that extends partial layout with missing
         labels in the original order.
         To do this naively: walk over the original layout and
         check for each pair of consecutive labels (prev, curr)
         if curr does not appear in partial layout, find prev
         in partial layout and insert curr after it.
         We break the original layout into sub-layouts such that
         only the first label in each sublayout appears in the
         partial layout, and then reorder based on the partial layout.
      *)
      let new_cfg_layout =
        List.group orig_cfg_layout
          ~break:(fun _prev cur -> Map.mem partial_cfg_map cur)
        |> List.sort
             ~compare:(fun l1 l2 ->
               let h1 = List.hd_exn l1 in
               let h2 = List.hd_exn l2 in
               let index1 = Map.find_exn partial_cfg_map h1 in
               let index2 = Map.find_exn partial_cfg_map h2 in
               Int.compare index1 index2)
        |> List.concat
      in
      print_list "new:" new_cfg_layout;
      (* Make sure the new layout is just a permutation.
         CR gyorsh: do we need to handle block duplication here? *)
      assert (List.length new_cfg_layout = List.length orig_cfg_layout);
      assert ((List.sort new_cfg_layout ~compare:Int.compare) =
              (List.sort orig_cfg_layout ~compare:Int.compare));
      Cfg.set_layout cfg new_cfg_layout
    end

let reorder algo cfg =
  match algo with
  | Identity -> cfg
  | Random -> reorder_random cfg
  | CachePlus -> Misc.fatal_error "Not implemented: cache+ reorder algorithm"
  | External layout -> reorder_layout cfg layout
