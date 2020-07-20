(* Adaptation of [1] to basic blocks, influenced by [2]. The use of LBR
   profile information for calculating basic-block level execution counts
   used here is based on algorithms described in [3]. Collection and decoding
   of profile information is inspired by [4].

   [1] Optimizing function placement for large-scale data-center
   applications. Guilherme Ottoni and Bertrand Maher. In Proceedings of the
   2017 International Symposium on Code Generation and Optimization (CGO
   2017).

   [2] BOLT: A Practical Binary Optimizer for Data Centers and Beyond. Maksim
   Panchenko, Rafael Auler, Bill Nell, and Guilherme Ottoni. In Proceedings
   of 2019 International Symposium on Code Generation and Optimization (CGO
   2019).

   [3] Taming Hardware Event Samples for Precise and Versatile Feedback
   Directed Optimizations. Dehao Chen, Neil Vachharajani, Robert Hundt,
   Xinliang D. Li, Stéphane Eranian, Wenguang Chen, Weimin Zheng Published in
   IEEE Transactions on Computers 2013

   [4] AutoFDO: automatic feedback-directed optimization for warehouse-scale
   applications. Dehao Chen, David Xinliang Li, and Tipp Moseley. 2016. In
   Proceedings of the 2016 International Symposium on Code Generation and
   Optimization (CGO '16). *)
open Core

(* CR-someday xclerc: the various operations in this files (sorting,
   extracting the argmin/argmax, etc) seem to indicate that we might
   want to switch from lists to e.g. heaps. However, the execution
   profile shows that the tool does not spend much time in the clustering,
   so this should be low-priority (or even just discarded). *)

let verbose = ref true

type clusterid = int [@@deriving compare]

type weight = int64 [@@deriving compare]

let entry_pos = 0

(* Invariant: the weight of a cluster is the sum of the weights of the data
   it represents. *)
(* Invariant: weights must be non-negative. *)
(* Invariant: position of the cluster is the smallest position of any item it
   represents. This is a heuristic, see [Note1] *)
type 'd cluster =
  { id : clusterid;  (** unique id of the cluster *)
    pos : int;  (** the smallest, index in the original layout *)
    weight : weight;  (** weight *)
    items : 'd list;  (** data items represented by this cluster. *)
    mutable can_be_merged : bool
        (** [a.can_be_merged] is [false] means that [a] cannot be placed in a
            cluster _after_ another [b] **)
  }

type edge =
  { src : clusterid;
    dst : clusterid;
    weight : weight
  }
[@@deriving compare]

(* Directed graph whose nodes are clusters. *)
type 'd t =
  { next_id : clusterid;
    (* used to create unique cluster ids *)
    clusters : 'd cluster list;
    edges : edge list;
    original_layout : 'd list
  }

let id_to_cluster t id = List.find_exn t.clusters ~f:(fun c -> c.id = id)

let _find t data =
  List.find t.clusters ~f:(fun c ->
      List.mem c.items data ~equal:(fun d1 d2 -> d1 = d2))

let init_layout original_layout execounts =
  let open Block_info in
  (* Makes a singleton cluster. data, id and pos must be unique *)
  let mk_node ~data ~weight ~pos ~id =
    assert (Int64.(weight >= 0L));

    (* Cluster that contains the entry position of the original layout cannot
       be merged *after* another cluster. *)
    let can_be_merged = not (pos = entry_pos) in
    { id; weight; items = [data]; pos; can_be_merged }
  in
  let mk_edge ~src ~dst ~weight =
    assert (Int64.(weight >= 0L));
    { src; dst; weight }
  in
  (* Initialize each block in its own cluster: cluster id is block's position
     in the original layout, data is block label, weight is block's execution
     count, or 0 if there is no info. *)
  let clusters =
    List.mapi original_layout ~f:(fun pos data ->
        let weight =
          match Cfg_info.get_block execounts data with
          | None -> 0L
          | Some block_info -> block_info.count
        in
        mk_node ~pos ~id:pos ~data ~weight)
  in
  (* Add all branch info *)
  let label2pos =
    List.foldi original_layout ~init:Int.Map.empty ~f:(fun i acc data ->
        Int.Map.add_exn acc ~key:data ~data:i)
  in
  let find_pos label = Map.find_exn label2pos label in
  let edges =
    Cfg_info.fold execounts ~init:[] ~f:(fun ~key:_ ~data:block_info acc ->
        let src = find_pos block_info.label in
        List.fold block_info.branches ~init:acc ~f:(fun acc b ->
            if b.intra then
              let dst = find_pos (Option.value_exn b.target_label) in
              let e =
                mk_edge ~src ~dst ~weight:b.taken
                (* CR-someday gyorsh: can we factor in mispredicted? *)
              in
              e :: acc
            else acc))
  in
  { next_id = List.length clusters; clusters; edges; original_layout }

(* Compare clusters using their weight, in descending order. Tie breaker
   using their position in the orginal layout, if available. Otherwise, using
   their ids which are unique. Clusters that cannot be merged are at the end,
   ordered amongst them in the same way. *)
let cluster_compare_frozen c1 c2 =
  if Bool.equal c1.can_be_merged c2.can_be_merged then
    let res = compare_weight c2.weight c1.weight in
    if res = 0 then
      let res = Int.compare c1.pos c2.pos in
      if res = 0 then Int.compare c1.id c2.id else res
    else res
  else if c1.can_be_merged then -1
  else 1

let cluster_compare_pos c1 c2 =
  let res = Int.compare c1.pos c2.pos in
  if res = 0 then
    let res = compare_weight c1.weight c2.weight in
    if res = 0 then Int.compare c1.id c2.id else res
  else res

let _get_cluster t id = List.find_exn t.clusters ~f:(fun c -> c.id = id)

(* Compare edges using weights, in descending order. Tie breaker on sources
   first, then on destinations. *)
let edge_compare e1 e2 =
  let res = compare_weight e2.weight e1.weight in
  if res = 0 then
    let res = compare_clusterid e1.src e2.src in
    if res = 0 then compare_clusterid e1.dst e2.dst else res
  else res

(* Merge two clusters, laying out their components in order as c1@c2. *)
let merge t c1 c2 =
  assert c2.can_be_merged;
  assert (c1.id <> c2.id);
  let id = t.next_id in
  let next_id = id + 1 in
  (* The new pos is the minimal pos of the two clusters we merged. *)
  let pos = min c1.pos c2.pos in
  let can_be_merged = not (pos = entry_pos) in
  let c =
    { id;
      pos;
      can_be_merged;
      weight = Int64.(c1.weight + c2.weight);
      (* For layout, preserve the order of the input items lists. *)
      items = c1.items @ c2.items
    }
  in
  (* Add the new cluster at the front and remove the c1 and c2. *)
  let clusters =
    c
    :: List.filter t.clusters ~f:(fun c ->
           not (c.id = c1.id || c.id = c2.id))
  in
  (* Find all edges with endpoints c1 and c2, and replace them with c. *)
  let updated, preserved =
    List.partition_map t.edges ~f:(fun edge ->
        let s = edge.src in
        let d = edge.dst in
        let src = if s = c1.id || s = c2.id then c.id else s in
        let dst = if d = c1.id || d = c2.id then c.id else d in
        if src = edge.src && dst = edge.dst then Second edge
        else First { src; dst; weight = edge.weight })
  in
  (* Update the weights of the edges whose endpoints were updated. *)
  (* Preserve the invariant that the edges are unique pairs of (src,dst).
     This is temporarily violated by the update above if for example the
     edges c1->c3 and c2->c3 are both present in the input. Merge them by
     adding their weights up. *)
  let sorted = List.sort updated ~compare:compare_edge in
  let merged =
    List.fold sorted ~init:[] ~f:(fun acc e1 ->
        if e1.src = e1.dst then (* remove self-loops *)
          acc
        else
          match acc with
          | e2 :: rest when e2.src = e1.src && e2.dst = e1.dst ->
              let e =
                { src = e1.src;
                  dst = e1.dst;
                  weight = Int64.(e1.weight + e2.weight)
                }
              in
              e :: rest
          | _ -> e1 :: acc)
  in
  let edges = merged @ preserved in
  { t with edges; clusters; next_id }

let find_max_pred t c =
  let max =
    List.fold t.edges ~init:None ~f:(fun max e ->
        if e.dst = c.id && (* ignore self loops *) not (e.src = c.id) then
          match max with
          | None -> Some e
          | Some me -> if edge_compare me e < 0 then Some e else max
        else max)
  in
  match max with
  | None -> None
  | Some max -> Some max.src

(* Order clusters by their execution counts, descending, tie breaker using
   original layout, as in cluster's compare function. Choose the cluster with
   the highest weight. Find its "most likely predecessor" cluster i.e., the
   predecessor with the highest edge weight, tie breaker using original
   layout. Merge the two clusters. Repeat until all clusters are merged into
   a single cluster. Return its data. *)
let optimize_layout original_layout execounts =
  let t = init_layout original_layout execounts in
  let len_clusters = List.length t.clusters in
  let len_layout = List.length t.original_layout in
  if not (len_layout = len_clusters) then
    Report.user_error "layout length doesn't match cluster length."
  else if len_layout = 0 then (
    if !verbose then printf "Optimize layout called with empty layout.\n";
    [] )
  else
    (* Invariant preserved: clusters that can be merged are ordered by weight
       and pos, followed by clusters that cannot be merged. The new cluster
       has the highest weight, amongst ones that can be merged, because it
       takes the previously highest weight cluster cur and adds a
       non-negative weight of pred to it. If a cluster has no predecessors,
       it is moved to the end. *)
    let clusters = List.sort t.clusters ~compare:cluster_compare_frozen in
    let t = { t with clusters } in
    (* CR-someday xclerc: it might be slightly more efficient to keep two
       collections here: the clusters that can be merged and those that
       cannot. *)
    let rec loop t step =
      match t.clusters with
      | [] -> []
      | c :: rest ->
          if c.can_be_merged then (
            if !verbose then printf "Step %d: merging cluster %d\n" step c.id;
            match find_max_pred t c with
            | None ->
                (* Cluster c is not reachable from within the function using
                   any of the edges that we have for clustering, i.e., edges
                   that have weights. Move c to the end of the layout, after
                   marking that it cannot be merged. *)
                if !verbose then printf "No predecessor for %d\n" c.id;
                c.can_be_merged <- false;
                let t = { t with clusters = rest @ [c] } in
                loop t (step + 1)
            | Some pred_id ->
                let pred = id_to_cluster t pred_id in
                if !verbose then
                  printf "Found pred %d weight=%Ld\n" pred.id pred.weight;
                let t = merge t pred c in
                loop t (step + 1) )
          else
            (* Cannot merge any more clusters. Sort the remaining clusters in
               original layout order. This guarantees that the entry is at
               the front. *)
            let print msg x =
              let x = List.map x ~f:(fun c -> c.items) in
              Printf.printf !"%s: %{sexp: int list list}\n" msg x
            in
            if !verbose then print "clusters" t.clusters;
            let clusters =
              List.sort t.clusters ~compare:cluster_compare_pos
            in
            (* Merge their lists *)
            if !verbose then print "sorted" clusters;
            let layout =
              List.concat_map clusters ~f:(fun c ->
                  List.sort ~compare:Int.compare c.items)
            in
            if !verbose then printf "Finished in %d steps\n" step;
            layout
    in
    loop t 0

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
