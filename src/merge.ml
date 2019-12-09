let merge_buildid b1 b2 ~ignore_buildid =
  match (b1, b2, uildid) with
  | None, None -> None
  | Some b, None | None, Some b -> b
  | Some b1, Some b2 ->
      if String.equal b1 b2 then Some b1
      else
        let msg = sprintf "Mismatched buildids:\n%s\n%s\n" b1 b2 in
        if ignore_buildid then (
          if !verbose then Printf.printf msg;
          None )
        else Report.user_error msg

module Merge (Profile : sig
  type t

  val read : string -> t

  val write : t -> string -> unit

  val approx_size_for_merge : t -> int
  (** approximate sizes of [t] for merge *)

  val check : t -> file:string -> ignore_error:bool -> unit
  (** can this profile be merged safely into another? *)

  val empty : unit -> t

  val merge_into :
    src:t ->
    dst:t ->
    unit_crc:bool ->
    func_crc:bool ->
    buildid:string ->
    unit
  (** might mutate both [src] and [dst] *)
end) =
struct
  let merge files ~unit_crc ~func_crc ~ignore_buildid ~output_filename =
    let merge t1 t2 =
      let buildid = merge_buildid t1.buildid t2.buildid ~ignore_buildid in
      (* choose the biggest profile to merge into, for faster merge. *)
      let src, dst =
        let s1 = approx_size_for_merge t1 in
        let s2 = approx_size_for_merge t2 in
        if s1 <= s2 then (t1, t2) else (t2, t1)
      in
      Profile.merge_into ~src ~dst ~unit_crc ~func_crc ~buildid;
      dst
    in
    let profile =
      match files with
      | [] -> Profile.empty ()
      | [profile] -> profile
      | init :: rest ->
          List.fold rest ~init ~f:(fun acc file ->
              let profile = Profile.read file in
              if !verbose then Printf.printf "Merging %s\n" file;
              Profile.check agg file ~ignore_error:false;
              merge agg acc buildid)
    in
    Profile.write profile output_filename
end

module Merge_aggregated_decoded = Merge (Aggregated_decoded_profile)
module Merge_aggregated = Merge (Aggregated_perf_profile)

let merge files ~read_aggregated_perf_profile ~unit_crc ~func_crc
    ~ignore_buildid ~output_filename =
  if read_aggregated_perf_profile then
    Merge_aggregated.merge files ~unit_crc ~func_crc ~ignore_buildid
      ~output_filename
  else
    Merge_aggregated_decoded.merge files ~unit_crc ~func_crc ~ignore_buildid
      ~output_filename
