open Core

let instr (i : Linear.instruction) = { i with dbg = Debuginfo.none }

let fundecl (f : Linear.fundecl) =
  { f with fun_dbg = Debuginfo.none; fun_body = instr f.fun_body }

let unit (ui : Linear_format.linear_unit_info) =
  let open Linear_format in
  let items =
    List.map ui.items ~f:(function
      | Func f -> Func (fundecl f)
      | d -> d)
  in
  { ui with items }
