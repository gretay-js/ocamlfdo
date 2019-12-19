open Core

let verbose = ref true

let extension = "fdo.org"

let last_id = ref 0

let names = Hashtbl.create (module String)

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

(* Some names come out too long. This function shortens them or depending on
   whether they are write-only, or need to be reused. *)
let get_filename ~name ~title ~sub =
  let filename = sprintf "%s-%s.%s" name title sub in
  if String.length name < 255 then filename
  else sprintf "%s-%d-%s.%s" (String.prefix name 200) (get_id name) title sub

let with_outchannel ~name ~title ~sub printer x =
  let filename = get_filename ~name ~title ~sub in
  let out_channel = Out_channel.create filename in
  Misc.try_finally
    (fun () -> printer out_channel x)
    ~always:(fun () -> Out_channel.close out_channel)

let with_ppf ~name ~title ~sub formatter x =
  let filename = get_filename ~name ~title ~sub in
  let out_channel = Out_channel.create filename in
  let ppf = Format.formatter_of_out_channel out_channel in
  Misc.try_finally
    (fun () -> formatter ppf x)
    ~always:(fun () ->
      Format.pp_print_flush ppf ();
      Out_channel.close out_channel)

let filename = sprintf "summary.%s" extension

let enabled = ref false

let log msg =
  if !verbose then printf "%s" msg;
  if !enabled then
    Out_channel.with_file ~append:true ~binary:false filename ~f:(fun oc ->
        Printf.fprintf oc "%s%s" msg
          (if String.is_suffix msg ~suffix:"\n" then "" else "\n"))

let start () =
  if !verbose then printf "Creating summary file %s\n" filename;
  enabled := true

let finish () =
  if !verbose then printf "Written summary to %s\n" filename;
  enabled := false

module Hint = struct
  type t = Old_profile

  let to_fmt = function
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
