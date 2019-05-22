(**************************************************************************)
(*                                                                        *)
(*                                 OCamlFDO                               *)
(*                                                                        *)
(*                     Greta Yorsh, Jane Street Europe                    *)
(*                                                                        *)
(*   Copyright 2019 Jane Street Group LLC                                 *)
(*                                                                        *)
(*   All rights reserved.  This file is distributed under the terms of    *)
(*   the GNU Lesser General Public License version 2.1, with the          *)
(*   special exception on linking described in the file LICENSE.          *)
(*                                                                        *)
(**************************************************************************)
(* Debug printing *)
(* CR gyorsh: if it ever goes into the compiler, it needs to use formatters
   properly. Otherwise, use with sexp. *)
open Cfg
let terminator ppf ti =
  Format.fprintf ppf "\n";
  match ti.desc with
  | Branch successors ->
    Format.fprintf ppf "Branch with %d successors:\n" (List.length successors);
    List.iter (fun (c,l)->
      match c with
      | Always -> Format.fprintf ppf "goto %d\n" l
      | Test c -> Format.fprintf ppf "if %a then goto %d\n"
                    (Printmach.test c) ti.arg l)
      successors
  | Switch labels ->
    Format.fprintf ppf "switch %a of\n" Printmach.reg ti.arg.(0);
    for i = 0 to Array.length labels - 1 do
      Format.fprintf ppf "case %d: goto %d\n" i labels.(i)
    done
  | Return -> Format.fprintf ppf "Return\n"
  | Raise _ -> Format.fprintf ppf "Raise\n"
  | Tailcall _ -> Format.fprintf ppf "Tailcall\n"

let cfg oc cfg layout ~basic_to_linear ~linearize_terminator =
  let block ppf label b =
    Format.fprintf ppf "\n%d:\n" label;
    let i = List.fold_right basic_to_linear b.body Linearize.end_instr in
    Printlinear.instr ppf i;
    Format.fprintf ppf "%d: " b.terminator.id;
    terminator ppf b.terminator;
    begin try
      let t = (linearize_terminator b.terminator) in
      Printlinear.instr ppf t
    with _ -> () end;
    Format.fprintf ppf "\npredecessors:";
    LabelSet.iter (fun l -> Format.fprintf ppf " %d" l)
      b.predecessors;
    Format.fprintf ppf "\nsuccessors:";
    List.iter (fun l -> Format.fprintf ppf " %d" l)
      (successor_labels b)
  in
  let ppf = Format.formatter_of_out_channel oc in
  Printf.fprintf oc "\n%s\n" cfg.fun_name;
  Printf.fprintf oc "layout.length=%d\n" (List.length layout);
  Printf.fprintf oc "blocks.length=%d\n" (Hashtbl.length cfg.blocks);
  List.iter (fun label ->
    let b = Hashtbl.find cfg.blocks label
    in block ppf label b)
    layout
