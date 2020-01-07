open Core
module C = Ocamlcfg.Cfg
module CL = Ocamlcfg.Cfg_with_layout

let verbose = ref false

type t =
  { (* map func id to cfg_info of that function. *)
    (* sparse, only holds functions that have cfg *and* execounts. *)
    (* logically it should be defined inside Func.t but it creates a cyclic
       dependency between . The advantage of the current design is smaller
       space that a Func takes if it doesn't have a cfg_info *)
    execounts : Cfg_info.t Int.Table.t
  }

let mk () = { execounts = Int.Table.create () }

let add t id info = Hashtbl.add_exn t.execounts ~key:id ~data:info

let find t id = Hashtbl.find t.execounts id

(* Compute detailed execution counts for function [name] using its CFG *)
(* Translate linear ids of this function's locations to cfg labels within
   this function, find the corresponding basic blocks and update their
   block_info. Perform lots of sanity checks to make sure the location of the
   execounts match the instructions in the cfg. *)
let create_cfg_info (p : Aggregated_decoded_profile.t) func cl =
  let get_loc addr = Hashtbl.find_exn p.addr2loc addr in
  let i = Cfg_info.create cl func in
  (* Associate instruction counts with basic blocks *)
  Hashtbl.iteri func.agg.instructions ~f:(fun ~key ~data ->
      let loc = get_loc key in
      Cfg_info.record_ip i ~loc ~data);

  (* Associate fall-through trace counts with basic blocks *)
  Hashtbl.iteri func.agg.traces ~f:(fun ~key ~data ->
      let from_addr, to_addr = key in
      let from_loc = get_loc from_addr in
      let to_loc = get_loc to_addr in
      Cfg_info.record_trace i ~from_loc ~to_loc ~data);

  (* report stats *)
  let m = Cfg_info.malformed_traces i in
  ( if Execount.(m > 0L) then
    let total_traces =
      List.fold (Hashtbl.data func.agg.traces) ~init:0L ~f:Int64.( + )
    in
    Report.logf "Found %Ld malformed traces out of %Ld (%.3f%%)\n" m
      total_traces
      (Report.percent (Execount.to_int_trunc m)
         (Execount.to_int_trunc total_traces)) );

  (* Associate branch counts with basic blocks *)
  Hashtbl.iteri func.agg.branches ~f:(fun ~key ~data ->
      let mispredicts =
        Option.value (Hashtbl.find func.agg.mispredicts key) ~default:0L
      in
      let from_addr, to_addr = key in
      let from_loc = get_loc from_addr in
      let to_loc = get_loc to_addr in
      Cfg_info.record_branch i ~from_loc ~to_loc ~data ~mispredicts);
  if !verbose then (
    Cfg_info.dump i;
    Cfg_info.dump_dot i "annot" );
  i

(* cfg_info can be saved to a file for later use. It is only useful for
   debugging. It would save recomputing the counters, but it adds another
   file per function or compilation unit. We can't write them to all to one
   file because jenga many processes runs in parallel, all of which might be
   accessing the same file for write. An alternative is a profile service. In
   any case, saving these profiles also adds complexity to the build rules,
   which would decided for every compilation unit based on the existence of
   cfg profile file, whether to read it or to write it. It is probably not
   worth it because recomputing the counters is fairly fast, and not a
   bottleneck. Similarly, we could save the computed layout to file, but it
   is not worth it as it does not take very long to compute it, and it may
   not be useful if the target binary is rebuild with different heuristics. *)

exception Found of Cfg_info.t

(** Look for profile for function using its [name]. If not found, look for
    profiles for [alternatives]. Apply the profile to the cfg to compute
    detailed execution counts for blocks and branches. *)
let create_cfg_info (p : Aggregated_decoded_profile.t) name cl ~alternatives
    =
  try
    let f s =
      match Hashtbl.find p.name2id s with
      | None -> ()
      | Some id ->
          let func = Hashtbl.find_exn p.functions id in
          if Int64.(func.count > 0L) && func.has_linearids then (
            Report.logf
              "Found profile for function %s with %Ld samples %s (func id = \
               %d)"
              name func.count
              (if String.equal name s then "" else "using near match " ^ s)
              id;
            if !verbose then (
              printf "compute_cfg_execounts for %s\n" name;
              CL.print_dot cl "execount" );
            let cfg_info =
              Profile.record_call ~accumulate:true "cfg_info" (fun () ->
                  create_cfg_info p func cl)
            in
            raise (Found cfg_info) )
    in
    List.iter ~f (name :: alternatives);
    if !verbose then printf "Not found profile info for %s with cfg.\n" name;
    None
  with Found cfg_info -> Some cfg_info
