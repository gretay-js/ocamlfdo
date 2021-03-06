open Core

let verbose = ref true

let buildid b1 b2 ~ignore_buildid =
  match (b1, b2) with
  | None, None -> None
  | Some b, None | None, Some b -> Some b
  | Some b1, Some b2 ->
      if String.equal b1 b2 then Some b1
      else if ignore_buildid then (
        if !verbose then Printf.printf "Mismatched buildids:\n%s\n%s\n" b1 b2;
        None )
      else Report.user_error "Mismatched buildids:\n%s\n%s\n" b1 b2

module type Profile = sig
  type t

  val read : string -> t

  val write : t -> string -> unit

  val approx_size : t -> int

  val merge_into :
    src:t -> dst:t -> crc_config:Crcs.Config.t -> ignore_buildid:bool -> unit
end

module type Algo = sig
  type profile

  val merge :
    profile ->
    profile ->
    crc_config:Crcs.Config.t ->
    ignore_buildid:bool ->
    profile

  val merge_files :
    string list ->
    crc_config:Crcs.Config.t ->
    ignore_buildid:bool ->
    output_filename:string ->
    unit
end

module Make (P : Profile) = struct
  type profile = P.t

  let merge t1 t2 ~crc_config ~ignore_buildid =
    (* choose the biggest profile to merge into, for faster merge. *)
    let src, dst =
      let s1 = P.approx_size t1 in
      let s2 = P.approx_size t2 in
      if s1 <= s2 then (t1, t2) else (t2, t1)
    in
    P.merge_into ~src ~dst ~crc_config ~ignore_buildid;
    dst

  (* CR-someday gyorsh: stable result, not influenced by the order in which
     the files are provided in the command line. The problem is that we would
     need to hold all of these profiles in memory at the same time, or read
     and parse them twice, to calculate their measurements. Alternatively, we
     can ensure that merge is symmetric. Currently, it is not but only in one
     way: the ids of functions. This is chosen for efficiency: to avoid
     allocating larger than necessary hashtbles and traversing larger than
     necessary. *)
  let merge_files files ~crc_config ~ignore_buildid ~output_filename =
    let profile =
      match files with
      | [] -> Report.user_error "Cannot merge, no input files"
      | file :: rest ->
          List.fold rest ~init:(P.read file) ~f:(fun acc file ->
              let profile = P.read file in
              if !verbose then Printf.printf "Merging %s\n" file;
              merge profile acc ~crc_config ~ignore_buildid)
    in
    P.write profile output_filename
end
