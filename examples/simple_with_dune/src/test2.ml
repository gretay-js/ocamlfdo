let max = 50000000
let foo () =
  let j = ref 1 in
  let i = ref 1 in
  while !j < max do
    i := !j;
    while not (!i = 1) do
      if ((!i) mod 2) = 0 then
        i := !i / 2
      else
        i := ((!i)*3) + 1
    done;
    (* Printf.printf "%d\n" !j; *)
    assert ((!i) = 1);
    j := !j + 1
  done;
  ()

let () =
  (* Printf.printf "start\n"; *)
  foo ();
  (* Printf.printf "end\n"; *)
