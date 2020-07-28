include Execount

let create cfg_info block =
  match cfg_info with
  | None -> zero
  | Some cfg_info ->
    match Cfg_info.get_block cfg_info block with
    | None -> zero
    | Some bf -> bf.count

let lub = Execount.max
