open Core

(* Less insane wrapper around the hardware register
 * numbers used by the Linear IR
 *)
module T = struct
  type t = int [@@deriving sexp, compare]
end

include T
module Set = Set.Make(T)
module Map = Map.Make(T)

let all_registers =
  (* Set of all available registers on the target *)
  let regs = ref Set.empty in
  for i = 0 to Proc.num_register_classes - 1 do
    for j = 0 to Proc.num_available_registers.(i) - 1 do
      regs := Set.add !regs (Proc.first_available_register.(i) + j)
    done
  done;
  !regs
