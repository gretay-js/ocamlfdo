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

module LabelSet : Set.S with type elt = label

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
  (* CR: make id into an abstract type to distinguish special cases of new
     ids explicitly. *)
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

(* CR gyorsh: Switch can be translated to Branch. *)
(* Control Flow Graph of a function. *)
type t = {
  blocks : (label, block) Hashtbl.t;
  (* Map labels to blocks *)
  fun_name : string;
  (* Function name, used for printing messages *)
  entry_label : label; (* Must be first in all layouts of this cfg. *)
}

val successors : block -> successor list

val successor_labels : block -> label list

(* Debug printing *)
val print :
  out_channel ->
  t ->
  label list ->
  basic_to_linear:(basic instruction ->
                  Linear.instruction ->
                  Linear.instruction) ->
  linearize_terminator:(terminator instruction -> Linear.instruction) ->
  unit

val print_terminator : Format.formatter -> terminator instruction -> unit
