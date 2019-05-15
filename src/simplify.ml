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
(* Simplify comparison operations. *)

(* CR gyorsh: Too complicated?
   This approach may be useful earlier in the compiler.
   Is there existing code in the compiler that does something similar? *)

open Cmm
module Rep = struct
  (* Represent a powerset of {eq,lt,gt,uo} as a "bitvector" of length 4. *)
  type bit = T | F
  type rep = { eq: bit;
               lt: bit;
               gt: bit;
               uo : bit;
             }
  type cint =
    | Always
    | Cint of integer_comparison

  type cfloat =
    | Always
    | Unordered (* for floating point comparison not representable in CMM *)
    | Cfloat of float_comparison

  let ( || ) b1 b2 = if b1 = T || b2 = T then T else F

  let disjunction c1 c2 =
    { eq = c1.eq || c2.eq;
      lt = c1.lt || c2.lt;
      gt = c1.gt || c2.gt;
      uo = c1.uo || c2.uo;
    }

  let cint_to_rep = function
    | Ceq -> { eq=T; lt=F; gt=F; uo=F; }
    | Clt -> { eq=F; lt=T; gt=F; uo=F; }
    | Cgt -> { eq=F; lt=F; gt=T; uo=F; }
    | Cne -> { eq=F; lt=T; gt=T; uo=F; }
    | Cle -> { eq=T; lt=T; gt=F; uo=F; }
    | Cge -> { eq=T; lt=F; gt=T; uo=F; }

  let cint_of_rep = function
    | { eq=T; lt=F; gt=F; uo=F; } -> Cint Ceq
    | { eq=F; lt=T; gt=F; uo=F; } -> Cint Clt
    | { eq=F; lt=F; gt=T; uo=F; } -> Cint Cgt
    | { eq=F; lt=T; gt=T; uo=F; } -> Cint Cne
    | { eq=T; lt=T; gt=F; uo=F; } -> Cint Cle
    | { eq=T; lt=F; gt=T; uo=F; } -> Cint Cge
    | { eq=T; lt=T; gt=T; uo=F; } -> Always
    | _ -> assert false

  let cfloat_to_rep = function
    | CFeq -> { eq=T; lt=F; gt=F; uo=F; }
    | CFlt -> { eq=F; lt=T; gt=F; uo=F; }
    | CFgt -> { eq=F; lt=F; gt=T; uo=F; }
    | CFle -> { eq=T; lt=T; gt=F; uo=F; }
    | CFge -> { eq=T; lt=F; gt=T; uo=F; }
    | CFneq -> { eq=F; lt=T; gt=T; uo=T; }
    | CFnlt -> { eq=T; lt=F; gt=T; uo=T; }
    | CFngt -> { eq=T; lt=T; gt=F; uo=T; }
    | CFnle -> { eq=F; lt=F; gt=T; uo=T; }
    | CFnge -> { eq=F; lt=T; gt=F; uo=T; }

  let cfloat_of_rep = function
    | { eq=T; lt=F; gt=F; uo=F; } -> Cfloat CFeq
    | { eq=F; lt=T; gt=F; uo=F; } -> Cfloat CFlt
    | { eq=F; lt=F; gt=T; uo=F; } -> Cfloat CFgt
    | { eq=T; lt=T; gt=F; uo=F; } -> Cfloat CFle
    | { eq=T; lt=F; gt=T; uo=F; } -> Cfloat CFge
    | { eq=F; lt=T; gt=T; uo=T; } -> Cfloat CFneq
    | { eq=T; lt=F; gt=T; uo=T; } -> Cfloat CFnlt
    | { eq=T; lt=T; gt=F; uo=T; } -> Cfloat CFngt
    | { eq=F; lt=F; gt=T; uo=T; } -> Cfloat CFnle
    | { eq=F; lt=T; gt=F; uo=T; } -> Cfloat CFnge
    | { eq=T; lt=T; gt=T; uo=T; } -> Always
    | { eq=F; lt=F; gt=F; uo=T; } -> Unordered
    | _ -> assert false

end

(* Compute one comparison operator that is equivalent to the disjunction
   of the operators c1 and c2.
   [f] is the constructor to apply to the simplified comparison operator,
   unless the result is "Always" which is not expressible as a comparison. *)
let simplify_disjunction_int c1 c2 f =
  let open Rep in
  let res = cint_of_rep
              (disjunction (cint_to_rep c1) (cint_to_rep c2)) in
  match res with
  | Cint c -> [ f c ]
  | Always -> [ Cfg.Always ]

let simplify_disjunction_float c1 c2 =
  let open Rep in
  let open Cfg in
  let res = cfloat_of_rep
              (disjunction (cfloat_to_rep c1) (cfloat_to_rep c2)) in
  match res with
  | Cfloat c -> [ Test (Ifloattest c) ]
  | Always -> [ Always ]
  | Unordered ->
    (* Not representable and emitter doesn't support it. *)
    [ Test (Ifloattest c2); Test (Ifloattest c2) ]

let disjunction cmp1 cmp2 =
  let open Cfg in
  match cmp1, cmp2 with
  | Always, _ | _, Always ->
    (* This can happen only as a result of a previous simplification. *)
    [Always]
  | (Test c1), (Test c2) -> begin
      if c1 = c2 then [(Test c1)]
      else if c1 = Linearize.invert_test c2 then [Always]
      else begin
        match c1, c2 with
        | Iinttest(Isigned cmp1), Iinttest(Isigned cmp2) ->
          simplify_disjunction_int cmp1 cmp2
            (fun cmp -> Test(Iinttest(Isigned cmp)))
        | Iinttest(Iunsigned cmp1), Iinttest(Iunsigned cmp2) ->
          simplify_disjunction_int cmp1 cmp2
            (fun cmp -> Test(Iinttest(Iunsigned cmp)))
        | Iinttest_imm(Isigned cmp1, n1), Iinttest_imm(Isigned cmp2, n2)
          when n1 = n2 ->
          simplify_disjunction_int cmp1 cmp2
            (fun cmp -> Test(Iinttest_imm(Isigned cmp, n1)))
        | Iinttest_imm(Iunsigned cmp1, n1), Iinttest_imm(Iunsigned cmp2, n2)
          when n1 = n2 ->
          simplify_disjunction_int cmp1 cmp2
            (fun cmp -> Test(Iinttest_imm(Iunsigned cmp, n1)))
        | Ifloattest(cmp1), Ifloattest(cmp2) ->
          simplify_disjunction_float cmp1 cmp2
        | _ -> assert false
      end
    end
