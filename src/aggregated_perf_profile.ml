open Core

let verbose = ref false

module P = struct
  (* Pair of addresses *)
  type t = Addr.t * Addr.t [@@deriving compare, hash, sexp]
end

type t = {
  instructions : Execount.t Hashtbl.M(Addr).t;
  branches : Execount.t Hashtbl.M(P).t;
  (* number of times the branch was taken. *)
  mispredicts : Execount.t Hashtbl.M(P).t;
  (* number of times the branch was mispredicted: branch target mispredicted
     or branch direction was mispredicted. *)
  traces : Execount.t Hashtbl.M(P).t;
      (* execution count: number of times the trace was taken. *)
}
[@@deriving sexp]

let empty () =
  {
    instructions = Hashtbl.create (module Addr);
    branches = Hashtbl.create (module P);
    mispredicts = Hashtbl.create (module P);
    traces = Hashtbl.create (module P);
  }

let read filename =
  if !verbose then
    printf "Reading aggregated perf profile from %s\n" filename;
  let t =
    match Parsexp_io.load (module Parsexp.Single) ~filename with
    | Ok t_sexp -> t_of_sexp t_sexp
    | Error error ->
        Parsexp.Parse_error.report Caml.Format.std_formatter error ~filename;
        failwith "Cannot parse aggregated profile file"
  in
  if !verbose then Printf.printf !"Aggregated perf profile:\n%{sexp:t}\n" t;
  t

let write t filename =
  if !verbose then printf "Writing aggregated perf profile to %s\n" filename;
  let chan = Out_channel.create filename in
  Printf.fprintf chan !"%{sexp:t}\n" t;
  Out_channel.close chan
