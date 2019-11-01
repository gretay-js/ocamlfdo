open Core

let verbose = ref true

type loc = {
  (* function name *)
  name : string;
  (* offset from function start *)
  offset : int;
  (* id decode from debug info *)
  id : int option;
  (* raw address, only computed if we found the symbol with name in the
     binary. used for creating aggregated_decoded_profile from bolt_profile.
     it's not necessary but we currently require Loc.t to have address. *)
  addr : Addr.t option;
}
[@@deriving sexp]

type branch = {
  src : loc option;
  dst : loc option;
  count : Execount.t;
  mis : Execount.t;
}
[@@deriving sexp]

type t = branch list [@@deriving sexp]

let _loc_to_string_long loc =
  match loc with
  | None -> "[unknown] 0 0"
  | Some loc ->
      sprintf "%s %x %d" loc.name loc.offset
        (Option.value loc.id ~default:(-1))

let _loc_to_string_long loc =
  match loc with
  | None -> "[unknown]"
  | Some loc ->
      sprintf "%s %d (%x)" loc.name
        (Option.value loc.id ~default:(-1))
        loc.offset

let loc_to_string_short loc =
  match loc with
  | None -> "[unknown]"
  | Some loc -> sprintf "%s %d" loc.name (Option.value loc.id ~default:(-1))

let loc_to_string = loc_to_string_short

let print_branch ~chan b =
  (* OCaml entry functions are marked as "reduce_size" and not "fast". We do
     not annotated them with linear ids and we do not compute CFG for them.
     Therefore, we cannot compute fallthrough for them. This mismatch is
     harder to detect using diff, so we mark these entries with a star at
     the end of the output. We mark with two stars entries that start with
     "caml_" and typically belong to the ocaml runtime. *)
  let print_notes = false in
  let entry_note =
    if print_notes then
      match (b.src, b.dst) with
      | Some src, Some dst
        when String.equal src.name dst.name
             && String.is_prefix ~prefix:"caml" src.name
             && String.is_suffix ~suffix:"__entry" src.name ->
          "*"
      | Some src, Some dst
        when String.equal src.name dst.name
             && String.is_prefix ~prefix:"caml_" src.name ->
          "**"
      | _ -> ""
    else ""
  in
  fprintf chan "%s %s %Ld %Ld %s\n" (loc_to_string b.src)
    (loc_to_string b.dst) b.mis b.count entry_note

let write t ~filename =
  if !verbose then printf "Writing decoded bolt profile to %s\n" filename;
  let chan = Out_channel.create filename in
  List.iter t ~f:(print_branch ~chan);
  Out_channel.close chan

(* Read bolt profile and decode it. For testing purposes, keep
   aggregated_decoded_profile separate from decoded_bolt_profile. *)
let create locations ~filename =
  (* parse bolt profile file *)
  let bolt_profile = Bolt_profile.read ~filename in
  (* Resolving dwarf functions and address is slow, so we batch it. *)
  (* Collect all function names *)
  let len = List.length bolt_profile in
  let functions = Hashtbl.create len in
  let add_name bolt_loc =
    match Bolt_profile.Bolt_loc.get_sym bolt_loc with
    | None -> ()
    | Some (name, _) ->
        if not (Hashtbl.mem functions name) then
          Hashtbl.add functions name None
  in
  let gather_function_names (bolt_branch : Bolt_profile.Bolt_branch.t) =
    add_name bolt_branch.src;
    add_name bolt_branch.dst
  in
  List.iter bolt_profile ~f:gather_function_names;
  Elf_locations.find_functions locations functions;

  (* Collect all addresses *)
  let addresses = Hashtbl.create len in
  let add bolt_loc =
    match Bolt_profile.Bolt_loc.get_sym bolt_loc with
    | None -> ()
    | Some (name, offset) -> (
        match Hashtbl.find functions name with
        | None -> ()
        | Some start ->
            let addr = Int64.(start + of_int offset) in
            Hashtbl.add addresses addr None )
  in
  let gather_addresses (bolt_branch : Bolt_profile.Bolt_branch.t) =
    add bolt_branch.src;
    add bolt_branch.dst
  in
  List.iter bolt_profile ~f:gather_addresses;
  Elf_locations.resolve_all locations addresses;

  (* Construct decoded *)
  let decode_loc bolt_loc =
    match Bolt_profile.Bolt_loc.get_sym bolt_loc with
    | None -> None
    | Some (name, offset) -> (
        match Hashtbl.find functions name with
        | None ->
            if String.is_suffix ~suffix:"@PLT" name then
              (* OCamlFDO doesn't know PLT names, so we ignore the LBR
                 entries in BOLT profile that carry them. This lets us
                 automate the comparison. *)
              None
            else Some { name; id = None; offset; addr = None }
        | Some start -> (
            let program_counter = Int64.(start + of_int offset) in
            let dbg = Hashtbl.find addresses program_counter in
            match dbg with
            | None ->
                Some
                  { name; id = None; offset; addr = Some program_counter }
            | Some dbg ->
                if
                  Filenames.match_filename Linear ~expected:name
                    ~actual:dbg.file
                then
                  Some
                    {
                      name;
                      id = Some dbg.line;
                      offset;
                      addr = Some program_counter;
                    }
                else
                  Some
                    { name; id = None; offset; addr = Some program_counter }
            ) )
  in
  List.fold bolt_profile ~init:[] ~f:(fun acc b ->
      match (decode_loc b.src, decode_loc b.dst) with
      | None, None -> acc
      | src, dst ->
          let d = { src; dst; count = b.count; mis = b.mis } in
          d :: acc)

(* create Aggregated_decoded_profile *)
let export t =
  let add table key count =
    Hashtbl.update table key ~f:(fun v ->
        Int64.(count + Option.value ~default:0L v))
  in
  let agg = Aggregated_perf_profile.empty () in
  List.iter t ~f:(fun b ->
      match (b.src, b.dst) with
      | Some src, Some dst -> (
          match (src.addr, dst.addr) with
          | Some src_addr, Some dst_addr ->
              let key = (src_addr, dst_addr) in
              add agg.branches key b.count;
              add agg.mispredicts key b.mis
          | _ -> () )
      | _ -> ());
  agg

let save (p : Aggregated_decoded_profile.t)
    (agg : Aggregated_perf_profile.t) ~filename =
  if !verbose then
    printf
      "Writing fallthrough from aggregated decoded profile in decoded bolt \
       form to %s\n"
      filename;
  let chan = Out_channel.create filename in
  let get_loc addr =
    let loc = Hashtbl.find_exn p.addr2loc addr in
    let id =
      match loc.dbg with
      | None -> None
      | Some dbg -> Some dbg.line
    in
    match loc.rel with
    | None -> None
    | Some rel ->
        let func = Hashtbl.find_exn p.functions rel.id in
        Some { name = func.name; offset = rel.offset; id; addr = None }
  in
  Hashtbl.iteri agg.branches ~f:(fun ~key ~data:count ->
      let mis =
        Option.value (Hashtbl.find agg.mispredicts key) ~default:0L
      in
      let src_addr, dst_addr = key in
      let src = get_loc src_addr in
      let dst = get_loc dst_addr in
      match (src, dst) with
      | None, None -> ()
      | _ ->
          let b = { src; dst; count; mis } in
          print_branch ~chan b);
  Out_channel.close chan

let save_fallthrough (p : Aggregated_decoded_profile.t) ~filename =
  if !verbose then
    printf
      "Writing fallthrough from aggregated decoded profile in decoded bolt \
       form to %s\n"
      filename;
  let chan = Out_channel.create filename in
  (* For each function, print inferred fallthrough edges. *)
  Hashtbl.iter p.functions ~f:(fun func ->
      match Hashtbl.find p.execounts func.id with
      | Some cfg_info when func.has_linearids ->
          Hashtbl.iter cfg_info ~f:(fun bi ->
              List.iter bi.branches ~f:(fun b ->
                  if b.fallthrough then
                    let count = b.taken in
                    let mis = b.mispredicts in
                    let src =
                      Some
                        {
                          name = func.name;
                          id = Some bi.terminator_id;
                          offset = 0;
                          addr = None;
                        }
                    in
                    let dst =
                      Some
                        {
                          name = func.name;
                          id = b.target_id;
                          offset = 0;
                          addr = None;
                        }
                    in
                    let b = { src; dst; mis; count } in
                    print_branch ~chan b))
      | _ -> ());
  Out_channel.close chan
