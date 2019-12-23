open Core

let print_version v =
  match v with
  | None -> "n/a"
  | Some v -> Build_info.V1.Version.to_string v

let print_lib l =
  Build_info.V1.Statically_linked_library.(
    Printf.sprintf "(%s %s)" (name l) (print_version (version l)))

let build_info =
  String.concat ~sep:"\n"
    (List.map
       (Build_info.V1.Statically_linked_libraries.to_list ())
       ~f:print_lib)

let version = print_version (Build_info.V1.version ())
