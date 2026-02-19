open ContainersLabels

type range =
  {start: int;
   stop: int}

let parse_input path =
  IO.with_in path @@ fun ic ->
  IO.read_all ic
  |> String.trim
  |> String.split_on_char ~sep:','
  |> List.map ~f:(fun s ->
      match String.split_on_char ~sep:'-' s with
      | [start_str; stop_str] ->
        let start = Int.of_string_exn start_str in
        let stop = Int.of_string_exn stop_str in
        {start; stop}
      | _ -> failwith "Wrong input")

(*
let _ = parse_input "../inputs/day02.txt"
*)

let log100 x = (log10 x) /. 2.

let is_even i = i mod 2 = 0
let is_odd i = i mod 2 <> 0

let get_lower_bound i =
  if i < 1 then failwith "Number should be strictly positive";
  let pow_floor = Float.of_int i |> log10 |> floor |> Int.of_float in
  if is_even pow_floor then
    Int.pow 10 (pow_floor / 2)
  else
    let cut_in_half = (pow_floor + 1) / 2 |> Int.pow 10 in
    let head = i / cut_in_half in
    let tail = i mod cut_in_half in
    if tail <= head then head else head + 1

(* TODO: can this be solved with log100? *)
let get_lower_bound' i =
  let log_i = Int.to_float i |> log100 in
  let pow_ceil = log_i |> ceil |> Float.to_int in
  let cut_in_half = Int.pow 10 pow_ceil in
  let head = i / cut_in_half in
  let tail = i mod cut_in_half in
  (log_i, head, tail)
[@@warning "-32"]

(*
let _ = List.map [111; 1005; 1050] ~f:(fun i -> (i, get_lower_bound' i))
*)

let get_upper_bound i =
  if i < 2 then failwith "Upper bound should at least be 2";
  let pow_floor = Float.of_int i |> log10 |> floor |> Int.of_float in
  if is_even pow_floor then
    Int.pow 10 (pow_floor / 2) |> pred
  else
    let cut_in_half = (pow_floor + 1) / 2 |> Int.pow 10 in
    let head = i / cut_in_half in
    let tail = i mod cut_in_half in
    if tail < head then (head - 1) else head

let get_part_1 input_path =
  parse_input input_path
  |> List.to_seq
  |> Seq.filter_map (fun {start; stop} ->
      let lower = get_lower_bound start in
      let upper = get_upper_bound stop in
      if lower > upper then None
      else Some (lower, upper))
  |> Seq.map (fun (from, until) -> Seq.range from until)
  |> Seq.flatten
  |> Seq.map (fun i -> (Int.to_string i) ^ (Int.to_string i) |> Int.of_string_exn)
  |> Seq.fold (+) 0
[@@warning "-32"]

(*
let _ = get_part_1 "../inputs/day02.txt"
*)

(*
let _ = parse_input "../inputs/day02.sample.txt"
*)

let get_pairs {start; stop} : (int * int) list =
  let to_pow_of_ten i = Int.to_float i |> log10 |> Int.of_float |> succ in
  let ten_pow_up = to_pow_of_ten stop in
  let rec get_next_pair acc i =
    let ten_pow_i = to_pow_of_ten i in
    if ten_pow_i = ten_pow_up then (i, stop) :: acc |> List.rev
    else
      let next_i = Int.pow 10 ten_pow_i in
      let acc' = (i, next_i - 1) :: acc in
      if i > stop then failwith "i cannot be greater than the 'up' parameter";
      get_next_pair acc' next_i
  in
  get_next_pair [] start

let get_input_pairs input =
  let open List in
  map input ~f:get_pairs
  |> flatten
[@@warning "-32"]

(*
let _ = [{start = 11; stop = 22222}] |> get_input_pairs
*)
