open Core

let verbose = ref false

type action =
  | Top_percent of Percent.t
  | Top_percent_samples of Percent.t
  | Top of int
  | Top_clusters of int
  | Min_samples of int
[@@deriving sexp]

type t = action list [@@deriving sexp]

let of_sexp s =
  let t = t_of_sexp s in
  if !verbose
  then Printf.printf !"input=%{sexp:Sexp.t}\nsexp=%{sexp:Sexp.t}\n" s (sexp_of_t t);
  List.iter t ~f:(function
      | Top_percent p
      | Top_percent_samples p ->
        let p = Percent.to_percentage p in
        if Float.O.(p < 0. || p > 100.)
        then
          Report.user_error
            "Cannot take %.0f%% of functions. Argument must be between 0%% and 100%%."
            p
      | Top _ | Top_clusters _ | Min_samples _ -> ());
  t
;;

let take_top_percent_samples l p =
  let total = List.fold l ~init:0L ~f:(fun acc (_, count) -> Execount.(acc + count)) in
  let max = Percent.apply p (Float.of_int64 total) |> Float.to_int64 in
  let list, sum =
    List.fold l ~init:([], 0L) ~f:(fun (list, sum) (name, count) ->
        if Execount.compare sum max < 0
        then (name, count) :: list, Execount.(sum + count)
        else list, sum)
  in
  if !verbose
  then
    printf
      "take_top_percent_samples %s of total %Ld = %Ld, found %Ld in top %d functions\n"
      (Percent.to_string p)
      total
      max
      sum
      (List.length list);
  List.rev list
;;

let apply_action l a =
  match a with
  | Top n ->
    if !verbose then Printf.printf "len before=%d\n" (List.length l);
    let l = List.take l n in
    if !verbose then Printf.printf "len after=%d\n" (List.length l);
    l
  | Top_percent p ->
    let total = List.length l in
    let n = Percent.apply p (Float.of_int total) |> Float.to_int in
    List.take l n
  | Top_percent_samples p -> take_top_percent_samples l p
  | Top_clusters _ -> failwith "Not implemented"
  | Min_samples n ->
    List.take_while l ~f:(fun (_, count) -> Execount.(compare count (of_int n)) >= 0)
;;

let apply t l = List.fold t ~init:l ~f:apply_action
