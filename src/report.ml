open Core

let verbose = ref false
let extension = "fdo.report"

let linear ~name title f =
  let filename = sprintf "%s-%s.linear.%s" name title extension in
  let out_channel = Out_channel.create filename in
  let ppf = Format.formatter_of_out_channel out_channel in
  Printlinear.fundecl ppf f;
  Out_channel.close out_channel

let cfg ~name title cfg =
  let filename = sprintf "%s-%s.cfg.%s" name title extension in
  let out_channel = Out_channel.create filename in
  Cfg_builder.print out_channel cfg;
  Out_channel.close out_channel

let msgs = ref []
let log msg =
  if !verbose then Printf.printf "%s" msg;
  msgs := msg::!msgs

let output () =
  let filename = sprintf "summary.%s" extension in
  let out_channel = Out_channel.create filename in
  List.iter !msgs ~f:(fun msg ->
    Printf.fprintf out_channel "%s%s" msg
      (if String.is_suffix msg ~suffix:"\n" then "" else "\n"));
  Out_channel.close out_channel

