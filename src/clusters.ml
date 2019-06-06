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
(*
   Adaptation of [1] to basic blocks, influenced by [2].
   The use of LBR profile information for calculating basic-block level
   execution counts used here is based on algorithms described in [3].
   Collection and decoding of profile information is inspired by [4].

   [1] Optimizing function placement for large-scale data-center applications.
   Guilherme Ottoni and Bertrand Maher.
   In Proceedings of the 2017 International Symposium on Code Generation and Optimization
   (CGO 2017).

   [2] BOLT: A Practical Binary Optimizer for Data Centers and Beyond.
   Maksim Panchenko, Rafael Auler, Bill Nell, and Guilherme Ottoni.
   In Proceedings of 2019 International Symposium on Code Generation and Optimization
   (CGO 2019).

   [3] Taming Hardware Event Samples for Precise and Versatile Feedback Directed
   Optimizations.
   Dehao Chen, Neil Vachharajani, Robert Hundt, Xinliang D. Li, Stéphane Eranian,
   Wenguang Chen, Weimin Zheng
   Published in IEEE Transactions on Computers 2013

   [4] AutoFDO: automatic feedback-directed optimization for warehouse-scale applications.
   Dehao Chen, David Xinliang Li, and Tipp Moseley. 2016.
   In Proceedings of the 2016 International Symposium on Code Generation and Optimization
   (CGO '16).
*)
open Core

let verbose = true

type clusterid = int
type weight = int64

let entry_pos = 0

(* Invariant: the weight of a cluster is the sum of the weights of
   the data it represents. *)
(* Invariant: weights must be non-negative. *)
(* Invariant: position of the cluster is the smallest position
   of any item it represents. This is a heuristic, see [Note1] *)
type 'd cluster = {
  id : clusterid; (* unique id of the cluster *)
  pos : int (* the smallest, index in the original layout *)
  weight : weight; (* weight *)
  items : 'd list; (* data items represented by this cluster. *)
}

type edge = {
  src : clusterid;
  dst : clusterid;
  weight : weight;
}

(* Directed graph whose nodes are clusters. *)
type t = {
  mutable next_id : clusterid; (* used to create unique cluster ids *)
  mutable clusters : 'd cluster list;
  mutable edges : edge list;
  original_layout : 'd list;
}

let init_layout original_layout =
  {
    next_id = 0;
    clusters = [];
    edges = [];
    original_layout;
  }

(* Invariant: next id is bigger than any cluster id in t *)
let get_fresh_id t =
  let id = t.next_id in
  t.next_id <- id + 1;
  id

(* Makes a singleton cluster, and adds it to the list,
   but does not preserve the order of the list.
   This function is intended only for construction of initial clusters. *)
let add_node t ~data ~weight =
  assert (weight >= 0);
  let id = get_fresh_id t in
  (* Find the position of this item the original layout *)
  let r = List.findi orig_cfg_layout ~f:(fun i l -> l = block.start) in
  let (pos,_) = Option.value_exn r in
  let c = { id; weight; items = [data]; pos; } in
  t.clusters <- c::t.clusters;

let add_edge t ~src ~dst ~weight =
  assert (weight >= 0);
  t.edges <- { src; dst; weight; }::t.edges

(* Compare clusters using their weight, in descending order.
   Tie breaker using their position in the orginal layout,
   if available. Otherwise, using their ids which are unique.
   Entry position is always first. All positions are non-negative. *)
let cluster_compare c1 c2 =
  if c1.pos = entry_pos || c2.pos = entry_pos then
    compare c1.pos c2.pos
  else begin
    let res = compare c2.weight c1.weight in
    if res = 0 then begin
      let res = compare c1.pos c2.pos in
      if res = 0 then compare c1.id c2.id
      else res
    end
    else res
  end

let get_cluster t id =
  List.find_exn t.clusters ~f:(fun c -> c.id = id)

(* Compare edges using weights, in descending order.
   Tie breaker on sources first, then on destinations. *)
let edge_compare t e1 e2 =
  let res = compare e2.weight e1.weight in
  if res = 0 then begin
    let res = compare (t.get_cluster e1.src) (t.get_cluster e2.src) in
    if res = 0 then compare (t.get_cluster e1.dst) (t.cluster e2.dst)
    else res
  end else res

(* Merge two clusters. *)
let merge t c1 c2 =
  let c = {
    id = get_fresh_id t;
    pos = min c1.pos c2.pos;
    weight=c1.weight + c2.weight;
    (* For layout, preserve the order of the input items lists. *)
    items = c1.items@c2.items;
  } in
  (* Add the new cluster at the front and remove the c1 and c2. *)
  t.clusters <-
    c::(List.filter t.clusters
          ~f:(fun c -> not (c.id = c1.id || c.id = c2.id)));
  (* Find all edges with endpoints c1 and c2, and replace them with c. *)
  let (updated,preserved) =
    List.partition_map t.edges
      ~f:(fun edge ->
        let s = edge.src in
        let d = edge.dst in
        let src =
          if s = c1.id || s = c2.id then c
          else s in
        let dst =
          if d = c1.id || d = c2.id then c
          else d in
        if src.id != edge.src || dst.id != edge.dst then
          `Fst {src;dst;weight=edge.weight}
        else
          `Snd edge
      ) in
  (* Update the weights of the edges whose endpoints were updated. *)
  (* Preserve the invariant that the edges are unique pairs of (src,dst).
     This is temporarily violated by the update above if for example
     the edges c1->c3 and c2->c3 are both present in the input.
     Merge them by adding their weights up.
  *)
  let sorted = List.sort updated ~compare:compare in
  let merged =
    List.fold sorted ~init:[]
      ~f:(fun acc e1 ->
        if e1.src = e1.dst then  (* remove self-loops *)
          acc
        else
          match acc with
          | e2::rest when e2.src = e1.src && e2.dst = e1.dst ->
            let e = {e1.src;e1.dst;weight=e1.weight+e2.weight} in
            e::rest
          | _ -> e1::acc
      ) in
  t.edges <- updated::preserved

let find_max_pred t c =
  let max =
    List.fold t.edges ~init:None
    ~f:(fun max e ->
      if e.dst = c.id then begin
        match max with
        | None -> e
        | Some me->
          if edge_compare me e < 0 then
            Some e
          else
            max
      end else max) in
  match max with
  | None -> None
  | Some -> Some max.src

(*
   Order clusters by their execution counts, descending,
   tie breaker using original layout, as in cluster's compare function.
   Choose the cluster with the highest weight.
   Find its "most likely predecessor" cluster i.e.,
   the predecessor with the highest edge weight,
   tie breaker using original layout.
   Merge the two clusters.
   Repeat untill all clusters are merged into a single cluster.
   Return its data.
*)
let optimize_layout t  =
  let len_clusters = List.length t.clusters in
  let len_layout = List.length t.original_layout in
  if not (len_layout = len_clusters) then begin
    failwith "layout length doesn't match cluster length."
  end else if len_layout = 0 then begin
    if verbose then
      printf "Optimize layout called with empty layout.\n";
    []
  end else begin
    (* Entry block is always first. *)

    (*
     Invariant preserved: clusters remain ordered by weight and pos.
     The new cluster has the highest weight,
     because it takes the previously highest weight cluster c2
     and adds a non-negative weight of c1 to it.
     The new pos is the minimal pos of the two clusters we merged.
    *)
    let entry_data = List.hd_exn layout in
    t.clusters <- List.sort t.clusters ~compare:cluster_compare;
    let rec loop t step =
      match t.clusters with
      | [] -> []
      | [c] ->
        if verbose then
          printf "Finished in %d steps\n" step;
        c.items
      | c::rest ->
        if verbose then
          printf "Step %d: merging cluster %d\n" step c.id;
        match find_max_pred t c with
        | None -> (* either c is entry block or its not reachable *)
          if verbose then
            printf "No predecessor for %d\n" c.id;
          failwith "not implemented"
        | Some pred ->
          if verbose then
            printf "Found pred %d weight=%d\n" pred.id pred.weight;
          merge t pred c;
          loop t step + 1
  end

(* [Note1] Position of cluster.
   This is a heuristic that orders merged clusters with equal weights
 * in a way that respects the original layout
 * of the data they contain as must as possible.
 * The goal is to preserve original layout's fallthroughs
 * if there is no profile for them, i.e., no strong indication
 * that they should be reordered.
 * An alternative is to set it to uninitialized_pos, which will order
 * all merged clusters before original clusters
 * because uninitialized_pos = -1 < pos from original layout.
 * This will result in hotter blocks ordered earlier. *)