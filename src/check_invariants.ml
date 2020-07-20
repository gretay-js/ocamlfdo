open Core

let strict = ref false

exception Not_equal_reg_array

let reg_array_equal ra1 ra2 =
  (* Uses stamps just like the implementation of compare for Reg.Set.t *)
  let reg_equal (r1 : Reg.t) (r2 : Reg.t) =
    if not (r1.stamp = r2.stamp) then raise Not_equal_reg_array
  in
  try
    Array.iter2_exn ~f:reg_equal ra1 ra2;
    true
  with Not_equal_reg_array -> false

let report_linear ~name title f =
  let filename = Report.get_filename ~name ~title ~sub:"lin" in
  let out_channel = Out_channel.create filename in
  let ppf = Format.formatter_of_out_channel out_channel in
  Misc.try_finally
    (fun () -> Printlinear.fundecl ppf f)
    ~always:(fun () ->
      Format.pp_print_flush ppf ();
      Out_channel.close out_channel)

let equal_linear f new_body msg =
  (* CR-someday gyorsh: we do not preserve live and dbg fields of some
     instructions, such as labels, and instruction that do not exist in cfg,
     like trap handling stuff, or things that CFG can generate new ones.

     For live, this is fine because this field is used for Lop but not for
     labels and trap handling instruciont. For dbg, it is fine for now
     because mshinwell said so.*)
  let ignored = function
    | Linear.Llabel _ | Lentertrap | Ladjust_trap_depth _ -> true
    | _ -> false
  in
  let open Linear in
  let rec equal i1 i2 =
    if
      Poly.equal i1.desc i2.desc
      && reg_array_equal i1.arg i2.arg
      && reg_array_equal i1.res i2.res
      && ( ignored i1.desc
         || Reg.Set.equal i1.live i2.live
            && Debuginfo.compare i1.dbg i2.dbg = 0 )
    then
      match i1.desc with
      | Lend -> true
      | _ -> equal i1.next i2.next
    else (
      Format.kasprintf prerr_endline "Equality failed in %s on:@;%a@;%a"
        f.fun_name Printlinear.instr i1 Printlinear.instr i2;
      false )
  in
  if not (equal f.fun_body new_body) then (
    let name = Filenames.to_symbol f.fun_name in
    (* Separate files for before and after to make it easier to diff *)
    report_linear ~name ("Before" ^ msg) f;
    report_linear ~name ("After" ^ msg) { f with fun_body = new_body };
    if !strict then
      Report.user_error
        "Conversion from linear to cfg and back to linear is not an \
         identity function %s.\n"
        name ();
    false )
  else true

let rec simplify i =
  let open Linear in
  let cont j = { j with next = simplify j.next } in
  let test c = Mach.Iinttest_imm (Iunsigned c, 1) in
  let module Label = Int in
  let simplify3way l0 l1 l2 ~fallthrough =
    let l0 = Option.value l0 ~default:fallthrough in
    let l1 = Option.value l1 ~default:fallthrough in
    let l2 = Option.value l2 ~default:fallthrough in
    match
      ( Label.equal l0 fallthrough,
        Label.equal l1 fallthrough,
        Label.equal l2 fallthrough )
    with
    | true, true, true -> simplify i.next
    | false, true, true ->
        { i with desc = Lcondbranch (test Clt, l0); next = simplify i.next }
    | true, false, true ->
        { i with desc = Lcondbranch (test Ceq, l1); next = simplify i.next }
    | true, true, false ->
        { i with desc = Lcondbranch (test Cgt, l2); next = simplify i.next }
    | false, false, true ->
        if Label.equal l0 l1 then
          { i with
            desc = Lcondbranch (test Cle, l0);
            next = simplify i.next
          }
        else cont i
    | false, true, false ->
        if Label.equal l0 l2 then
          { i with
            desc = Lcondbranch (test Cne, l0);
            next = simplify i.next
          }
        else cont i
    | true, false, false ->
        if Label.equal l1 l2 then
          { i with
            desc = Lcondbranch (test Cge, l1);
            next = simplify i.next
          }
        else cont i
    | false, false, false ->
        (* CR-someday gyorsh: this is very repetitive and can be simplified
           using Linear.invert_test . *)
        if Label.equal l0 l1 && Label.equal l0 l2 then
          { i with desc = Lbranch l0; next = simplify i.next }
        else if Label.equal l0 l1 then
          (* heuristic in cfg_to_linear is to emit unconditional branch to
             the smallest *)
          if Label.compare l2 l0 < 0 then
            let next =
              { i with desc = Lbranch l2; next = simplify i.next }
            in
            { i with desc = Lcondbranch (test Cle, l0); next }
          else
            let next =
              { i with desc = Lbranch l0; next = simplify i.next }
            in
            { i with desc = Lcondbranch (test Cgt, l2); next }
        else if Label.equal l1 l2 then
          if Label.compare l0 l1 < 0 then
            let next =
              { i with desc = Lbranch l0; next = simplify i.next }
            in
            { i with desc = Lcondbranch (test Cge, l1); next }
          else
            let next =
              { i with desc = Lbranch l1; next = simplify i.next }
            in
            { i with desc = Lcondbranch (test Clt, l0); next }
        else if Label.equal l0 l2 then
          if Label.compare l1 l0 < 0 then
            let next =
              { i with desc = Lbranch l1; next = simplify i.next }
            in
            { i with desc = Lcondbranch (test Cne, l0); next }
          else
            let next =
              { i with desc = Lbranch l0; next = simplify i.next }
            in
            { i with desc = Lcondbranch (test Ceq, l1); next }
        else cont i
  in
  match i.desc with
  | Lend -> i
  | Lcondbranch (_, lbl) | Lbranch lbl -> (
      match i.next.desc with
      | Llabel fallthrough when lbl = fallthrough -> simplify i.next
      | _ -> cont i )
  | Lcondbranch3 (l0, l1, l2) -> (
      match i.next.desc with
      | Llabel fallthrough -> simplify3way l0 l1 l2 ~fallthrough
      | _ -> cont i )
  | _ -> cont i
