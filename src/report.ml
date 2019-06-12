open Core

let verbose = ref false

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

let get_filename name title sub =
  let filename = sprintf "%s-%s.%s.%s" name title sub extension in
  if String.length name < 255 then filename
  else
    sprintf "%s-%d-%s.%s.%s" (String.prefix name 200) (get_id name) title
      sub extension

let linear ~name title f =
  let filename = get_filename name title "lin" in
  let out_channel = Out_channel.create filename in
  let ppf = Format.formatter_of_out_channel out_channel in
  Printlinear.fundecl ppf f;
  Out_channel.close out_channel

let cfg ~name title cfg =
  let filename = get_filename name title "cfg" in
  let out_channel = Out_channel.create filename in
  Cfg_builder.print out_channel cfg;
  Out_channel.close out_channel

let filename = sprintf "summary.%s" extension

let out_channel = Out_channel.create filename

let log msg =
  if !verbose then printf "%s" msg;
  Printf.fprintf out_channel "%s%s" msg
    (if String.is_suffix msg ~suffix:"\n" then "" else "\n")

let finish () =
  if !verbose then printf "Written summary to %s\n" filename;
  Out_channel.close out_channel
