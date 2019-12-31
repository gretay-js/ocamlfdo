open Core
open Loc

let verbose = ref true

module Kind = struct
  type t =
    | UnknownSymbol
    | Symbol
    | UnknownMem
    | Mem
  [@@deriving sexp, equal]

  let to_string = function
    | UnknownSymbol -> "0"
    | Symbol -> "1"
    | UnknownMem -> "3"
    | Mem -> "4"

  let of_string = function
    | "0" -> UnknownSymbol
    | "1" -> Symbol
    | "3" -> UnknownMem
    | "4" -> Mem
    | _ -> assert false
end

module Bolt_loc = struct
  type t =
    { kind : Kind.t;
      name : string;
      offset : int
    }
  [@@deriving sexp]

  let to_string t =
    sprintf "%s %s %x" (Kind.to_string t.kind) t.name t.offset

  let of_strings k name o =
    (* For local symbols, bolt appends "/1" "/2" etc. We remove it to match
       up against the original names in the binary for decoding. *)
    let name =
      match String.rsplit2 ~on:'/' name with
      | None -> name
      | Some (name, suffix) ->
          let n = Int.of_string suffix in
          assert (n > 0);
          name
    in
    { kind = Kind.of_string k; name; offset = Int.Hex.of_string ("0x" ^ o) }

  let unknown = { kind = UnknownSymbol; name = "[unknown]"; offset = 0 }

  let get_sym l =
    match l.kind with
    | Symbol -> Some (l.name, l.offset)
    | _ -> None
end

module Bolt_branch = struct
  type t =
    { src : Bolt_loc.t;
      dst : Bolt_loc.t;
      mis : Int64.t;
      count : Int64.t
    }
  [@@deriving sexp]

  let print ~chan t =
    fprintf chan "%s %s %Ld %Ld\n"
      (Bolt_loc.to_string t.src)
      (Bolt_loc.to_string t.dst)
      t.mis t.count

  let of_string s =
    match String.split ~on:' ' s with
    | [ src_kind;
        src_name;
        src_offset;
        dst_kind;
        dst_name;
        dst_offset;
        mis;
        count ] ->
        { src = Bolt_loc.of_strings src_kind src_name src_offset;
          dst = Bolt_loc.of_strings dst_kind dst_name dst_offset;
          mis = Int64.of_string mis;
          count = Int64.of_string count
        }
    | _ -> assert false
end

type t = Bolt_branch.t list [@@deriving sexp]

let print t = printf !"%{sexp:t}\n" t

let read ~filename =
  if !verbose then printf "Reading bolt fdata profile from %s\n" filename;
  let chan = In_channel.create filename in
  let t =
    In_channel.fold_lines chan ~init:[] ~f:(fun acc r ->
        let b = Bolt_branch.of_string r in
        b :: acc)
  in
  In_channel.close chan;
  if !verbose then print t;
  t

let write t ~filename =
  if !verbose then
    printf
      "Writing preliminary aggregated decoded profile in bolt form to %s\n"
      filename;
  let chan = Out_channel.create filename in
  List.iter t ~f:Bolt_branch.(print ~chan);
  Out_channel.close chan

let find_exn id2name id =
  let res = Map.find_exn id2name id in
  assert (List.length res = 1);
  List.hd_exn res

(* For each function, check its execounts and collect inferred fallthrough
   edges. *)
let fallthroughs locations (p : Aggregated_decoded_profile.t)
    (lp : Linearid_profile.t) id2name =
  (* Try to get the raw address in the original binary. We could try to
     create Loc.t with it. Create Bolt_loc.t with it directly because it is
     only used for debugging to generating bolt fdata. *)
  let inverse name line =
    let file = Filenames.(make Linear name) in
    Elf_locations.to_address locations { file; line }
  in
  let make_bolt_loc (func : Func.t) linearid =
    let func_name = find_exn id2name func.id in
    match inverse func_name linearid with
    | None -> Bolt_loc.unknown
    | Some addr ->
        { kind = Symbol;
          name = func_name;
          offset = Int64.(to_int_trunc (addr - func.start))
        }
  in
  Hashtbl.fold p.functions ~init:[] ~f:(fun ~key:_ ~data:func acc ->
      match Hashtbl.find lp.execounts func.id with
      | Some cfg_info when func.has_linearids ->
          (* This is expensive because we need to scan the entire original
             binary to find the function. *)
          (* cache all addresses within this function with their mapping to
             dwarf and its inverse. *)
          Elf_locations.resolve_range locations ~start:func.start
            ~finish:func.finish ~with_inverse:true;
          Hashtbl.fold cfg_info ~init:acc ~f:(fun ~key:_ ~data:bi acc ->
              List.fold bi.branches ~init:acc ~f:(fun acc b ->
                  if b.fallthrough then (
                    let src_id = bi.terminator_id in
                    let dst_id = Option.value_exn b.target_id in
                    let src = make_bolt_loc func src_id in
                    let dst = make_bolt_loc func dst_id in
                    let count = b.taken in
                    let mis = b.mispredicts in
                    if !verbose then
                      printf
                        "Found fallthrough in %s at linearids %d->%d \
                         count=%Ld\n"
                        (find_exn id2name func.id)
                        src_id dst_id count;
                    let boltb = { Bolt_branch.src; dst; count; mis } in
                    boltb :: acc )
                  else acc))
      | _ -> acc)

(* Make and fold using init and f *)
let mk (p : Aggregated_decoded_profile.t) (lp : Linearid_profile.t)
    (agg : Aggregated_perf_profile.t) locations ~init ~f =
  let id2name = Aggregated_decoded_profile.id2name p in
  let get_bolt_loc addr =
    let loc = Hashtbl.find_exn p.addr2loc addr in
    match loc.rel with
    | None -> Bolt_loc.unknown
    | Some rel ->
        { kind = Symbol;
          name = find_exn id2name rel.id;
          offset = rel.offset
        }
  in
  let append_if_valid acc b =
    let open Bolt_branch in
    if
      Kind.equal b.src.kind UnknownSymbol
      && Kind.equal b.dst.kind UnknownSymbol
    then acc
    else f acc b
  in
  let t =
    Hashtbl.fold agg.branches ~init ~f:(fun ~key ~data:count acc ->
        let mis =
          Option.value (Hashtbl.find agg.mispredicts key) ~default:0L
        in
        let src_addr, dst_addr = key in
        let src = get_bolt_loc src_addr in
        let dst = get_bolt_loc dst_addr in
        let b = { Bolt_branch.src; dst; mis; count } in
        append_if_valid acc b)
  in
  let ft = fallthroughs locations p lp id2name in
  if !verbose then (
    printf "fallthroughs:\n";
    List.iter ft ~f:(Bolt_branch.print ~chan:Out_channel.stdout);
    printf "\n" );
  List.fold ft ~init:t ~f:append_if_valid

let create (p : Aggregated_decoded_profile.t) (lp : Linearid_profile.t)
    (agg : Aggregated_perf_profile.t) locations =
  mk p lp agg locations ~init:[] ~f:(fun acc b -> b :: acc)

(* If [t] uses too much memory and we don't need it other than for printing,
   then we can print during [mk] instead of "append_if" above *)
let save p lp agg locations ~filename =
  if !verbose then
    printf
      "Writing preliminary aggregated decoded profile in bolt form to %s\n"
      filename;
  let chan = Out_channel.create filename in
  mk p lp agg locations ~init:() ~f:(fun _ b -> Bolt_branch.print ~chan b);
  Out_channel.close chan

(* If [t] uses too much memory and we don't need it other than for printing,
   then we can print during [mk] instead of "append_if" above *)
let save_fallthrough p lp locations ~filename =
  if !verbose then
    printf "Writing fallthroughs profile in bolt form to %s\n" filename;
  let chan = Out_channel.create filename in
  mk p lp (Aggregated_perf_profile.empty ()) locations ~init:()
    ~f:(fun _ b -> Bolt_branch.print ~chan b);
  Out_channel.close chan
