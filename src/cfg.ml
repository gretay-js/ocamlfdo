(**************************************************************************)
(*                                                                        *)
(*                                 OCamlFDO                               *)
(*                                                                        *)
(*                     Greta Yorsh, Jane Street Europe                    *)
(*                                                                        *)
(*   Copyright 2019 Jane Street Group LLC                                 *)
(*                                                                        *)
(*                         based on the work of                           *)
(*             Xavier Leroy, projet Cristal, INRIA Rocquencourt           *)
(*                                                                        *)
(*   Copyright 1996 Institut National de Recherche en Informatique et     *)
(*     en Automatique.                                                    *)
(*                                                                        *)
(*   All rights reserved.  This file is distributed under the terms of    *)
(*   the GNU Lesser General Public License version 2.1, with the          *)
(*   special exception on linking described in the file LICENSE.          *)
(*                                                                        *)
(**************************************************************************)
type label = Linear.label

module LabelSet = Set.Make (struct
  type t = label

  let compare (x : t) y = compare x y
end)

(* CR gyorsh: store label after separately and update after reordering. *)
type func_call_operation =
  | Indirect of { label_after : label }
  | Immediate of {
      func : string;
      label_after : label;
    }

type prim_call_operation =
  | External of {
      func : string;
      alloc : bool;
      label_after : label;
    }
  | Alloc of {
      bytes : int;
      label_after_call_gc : label option;
      spacetime_index : int;
    }
  | Checkbound of {
      immediate : int option;
      label_after_error : label option;
      spacetime_index : int;
    }

type operation =
  | Move
  | Spill
  | Reload
  | Const_int of nativeint
  | Const_float of int64
  | Const_symbol of string
  | Stackoffset of int
  | Load of Cmm.memory_chunk * Arch.addressing_mode
  | Store of Cmm.memory_chunk * Arch.addressing_mode * bool
  | Intop of Mach.integer_operation
  | Intop_imm of Mach.integer_operation * int
  | Negf
  | Absf
  | Addf
  | Subf
  | Mulf
  | Divf
  | Floatofint
  | Intoffloat
  | Specific of Arch.specific_operation
  | Name_for_debugger of {
      ident : Ident.t;
      which_parameter : int option;
      provenance : unit option;
      is_assignment : bool;
    }

type call_operation =
  | P of prim_call_operation
  | F of func_call_operation

type condition =
  | Always
  | Test of Mach.test

type successor = condition * label

(* CR gyorsh: Switch has successors but currently no way to attach User_data
   to them. Can be fixed by translating Switch to Branch. *)

(* basic block *)
type block = {
  start : label;
  mutable body : basic instruction list;
  mutable terminator : terminator instruction;
  mutable predecessors : LabelSet.t;
}

and 'a instruction = {
  desc : 'a;
  arg : Reg.t array;
  res : Reg.t array;
  dbg : Debuginfo.t;
  live : Reg.Set.t;
  trap_depth : int;
  id : int;
}

and basic =
  | Op of operation
  | Call of call_operation
  | Reloadretaddr
  | Entertrap
  | Pushtrap of { lbl_handler : label }
  | Poptrap
  | Prologue

and terminator =
  | Branch of successor list
  | Switch of label array
  | Return
  | Raise of Cmm.raise_kind
  | Tailcall of func_call_operation

(* Control Flow Graph of a function. *)
type t = {
  blocks : (label, block) Hashtbl.t;
  (* Map labels to blocks *)
  fun_name : string;
  (* Function name, used for printing messages *)
  entry_label : label; (* Must be first in all layouts of this cfg. *)
}

let successors block =
  match block.terminator.desc with
  | Branch successors -> successors
  | Return -> []
  | Raise _ -> []
  | Tailcall _ -> []
  | Switch labels ->
      Array.mapi
        (fun i label -> (Test (Iinttest_imm (Iunsigned Ceq, i)), label))
        labels
      |> Array.to_list

let successor_labels block =
  let _, labels = List.split (successors block) in
  labels

let print_terminator ppf ti =
  Format.fprintf ppf "\n";
  match ti.desc with
  | Branch successors ->
      Format.fprintf ppf "Branch with %d successors:\n"
        (List.length successors);
      List.iter
        (fun (c, l) ->
          match c with
          | Always -> Format.fprintf ppf "goto %d\n" l
          | Test c ->
              Format.fprintf ppf "if %a then goto %d\n" (Printmach.test c)
                ti.arg l)
        successors
  | Switch labels ->
      Format.fprintf ppf "switch %a of\n" Printmach.reg ti.arg.(0);
      for i = 0 to Array.length labels - 1 do
        Format.fprintf ppf "case %d: goto %d\n" i labels.(i)
      done
  | Return -> Format.fprintf ppf "Return\n"
  | Raise _ -> Format.fprintf ppf "Raise\n"
  | Tailcall _ -> Format.fprintf ppf "Tailcall\n"

let print_block ppf label b ~basic_to_linear ~linearize_terminator =
  Format.fprintf ppf "\n%d:\n" label;
  let i = List.fold_right basic_to_linear b.body Linear.end_instr in
  Printlinear.instr ppf i;
  Format.fprintf ppf "%d: " b.terminator.id;
  print_terminator ppf b.terminator;
  ( try
      let t = linearize_terminator b.terminator in
      Printlinear.instr ppf t
    with _ -> () );
  Format.fprintf ppf "\npredecessors:";
  LabelSet.iter (fun l -> Format.fprintf ppf " %d" l) b.predecessors;
  Format.fprintf ppf "\nsuccessors:";
  List.iter (fun l -> Format.fprintf ppf " %d" l) (successor_labels b)

(* CR gyorsh: add dot format output *)
let print oc cfg layout ~basic_to_linear ~linearize_terminator =
  let ppf = Format.formatter_of_out_channel oc in
  Printf.fprintf oc "\n%s\n" cfg.fun_name;
  Printf.fprintf oc "layout.length=%d\n" (List.length layout);
  Printf.fprintf oc "blocks.length=%d\n" (Hashtbl.length cfg.blocks);
  List.iter
    (fun label ->
      let b = Hashtbl.find cfg.blocks label in
      print_block ppf label b ~basic_to_linear ~linearize_terminator)
    layout
