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
(* CR gyorsh: add dot format output *)
open Cfg

val cfg
  : out_channel
  -> t
  -> label list
  -> basic_to_linear :
       (basic instruction -> Linearize.instruction -> Linearize.instruction)
  -> linearize_terminator : (terminator instruction -> Linearize.instruction)
  -> unit

val terminator : Format.formatter -> terminator instruction -> unit
