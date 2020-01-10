open Core

let verbose = ref true

let extension = "fdo.org"

let last_id = ref 0

let names = String.Table.create ()

let save_names = false

let get_id name =
  if save_names then
    match Hashtbl.find names name with
    | None ->
        Hashtbl.add_exn names ~key:name ~data:!last_id;
        incr last_id;
        !last_id - 1
    | Some id -> id
  else (
    incr last_id;
    !last_id - 1 )

(* depending on whether the name is write-only, or need to be reused, we can
   save ids associated with each name. *)
let get_filename ~name ~title ~sub =
  let filename = sprintf "%s-%s.%s" name title sub in
  if String.length name < 255 then filename
  else sprintf "%s-%d-%s.%s" (String.prefix name 200) (get_id name) title sub

let filename = sprintf "summary.%s" extension

let enabled = ref false

let percent part total =
  if total > 0 then Float.(100. *. (of_int part /. of_int total)) else 0.

let timestamp () = Time.to_string (Time.now ())

let log msg =
  if !verbose then printf "%s" msg;
  if !enabled then
    Out_channel.with_file ~append:true ~binary:false filename ~f:(fun oc ->
        Printf.fprintf oc "%s%s" msg
          (if String.is_suffix msg ~suffix:"\n" then "" else "\n"))

let logf fmt =
  Format.kasprintf
    (fun msg -> log msg)
    ("@?%s: " ^^ fmt ^^ "@.")
    (timestamp ())

let start () =
  if !verbose then printf "Creating summary file %s\n" filename;
  enabled := true

let finish () =
  if !verbose then printf "Written summary to %s\n" filename;
  enabled := false

module Hint = struct
  type t =
    | Old_profile
    | Mismatch

  let to_fmt = function
    | Mismatch ->
        format_of_string
          "Cannot apply the profile to code because the source code changed \
           and md5 check is disabled.\n\
           Try generating a new profile or use command line flag \
           -on-md5-mismatch skip -on-md5-missing skip."
    | Old_profile ->
        format_of_string
          "Profile format may have changed.\n\
           If you are using an old profile, try generating a new one."
end

let user_error ?(hint = None) ?(exn = None) fmt =
  let fmt_hint =
    match hint with
    | None -> fmt
    | Some h -> fmt ^^ "\nHint: " ^^ Hint.to_fmt h
  in
  Format.kfprintf
    (fun _ ->
      match exn with
      | None -> exit 321
      | Some exn -> raise exn)
    Format.err_formatter
    ("@?Error: " ^^ fmt_hint ^^ "@.")
