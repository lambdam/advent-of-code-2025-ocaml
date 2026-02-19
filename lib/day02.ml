open ContainersLabels

(*
---
Part 1
*)

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
let _ = parse_input "../inputs/day02.sample.txt"
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

(*
---
Part 2
*)

type input_pair =
  {lower: int;
   upper: int;
   digits: int;}

let get_pairs {start; stop} : input_pair list =
  let to_pow_of_ten i = Int.to_float i |> log10 |> Int.of_float |> succ in
  let ten_pow_up = to_pow_of_ten stop in
  let rec get_next_pair acc i =
    let ten_pow_i = to_pow_of_ten i in
    if ten_pow_i = ten_pow_up then {lower = i; upper = stop; digits = ten_pow_i} :: acc |> List.rev
    else
      let next_i = Int.pow 10 ten_pow_i in
      let acc' = {lower = i; upper = (next_i - 1); digits = ten_pow_i} :: acc in
      if i > stop then failwith "I cannot be greater than the 'up' parameter";
      (get_next_pair [@tailcall]) acc' next_i
  in
  get_next_pair [] start

let get_input_pairs input =
  let open List in
  map input ~f:get_pairs

(*
let _ = [{start = 11; stop = 22222}] |> get_input_pairs
let _ = parse_input "../inputs/day02.sample.txt" |> get_input_pairs
let _ = parse_input "../inputs/day02.txt" |> get_input_pairs
*)

let get_divisors (n: int) =
  let not_equal_to_n = (<>) n in
  Seq.fold_left (fun acc x -> if n mod x = 0 then (x :: acc) else acc)
    [] (Seq.range 1 n)
  |> List.filter ~f:not_equal_to_n
  |> List.rev

(*
let _ = get_divisors 5
let _ = get_divisors 9
let _ = get_divisors 12
*)

let format_number num_pieces piece =
  piece
  |> Int.to_string |> fun s -> String.repeat s num_pieces
  |> Int.of_string |> Option.get

(*
let _ = format_number 4 12
*)

let get_lower_bound_2 ~i ~digits ~divisor =
  if i < 1 then failwith "Number should be strictly positive";
  let rec get_lower_bound_2' i' digits' =
    let first_order = Int.pow 10 (digits' - divisor) in
    let second_order = Int.pow 10 (digits' - divisor - divisor) in
    let first_piece = i' / first_order in
    let second_piece = i mod first_order |> fun x -> x / second_order in
    (* Printf.printf "first: %d | second: %d\n" first_piece second_piece; *)
    if first_piece < second_piece then first_piece + 1
    else if first_piece > second_piece then first_piece
    else if first_piece = second_piece && digits' = divisor * 2 then first_piece
    else if first_piece = second_piece && digits' > divisor * 2 then
      (get_lower_bound_2' [@tailcall]) (i mod first_order) (digits' - divisor)
    else failwith "Unhandled case in get_lower_bound_2"
  in
  get_lower_bound_2' i digits

(*
let _ = get_lower_bound_2 ~i:824824821 ~digits:9 ~divisor:3
let _ = get_lower_bound_2 ~i:824824824 ~digits:9 ~divisor:3
let _ = get_lower_bound_2 ~i:824824936 ~digits:9 ~divisor:3
let _ = get_lower_bound_2 ~i:999 ~digits:3 ~divisor:1
*)

let get_upper_bound_2 ~i ~digits ~divisor =
  if i < 1 then failwith "Number should be strictly positive";
  let rec get_upper_bound_2' i' digits' =
    let first_order = Int.pow 10 (digits' - divisor) in
    let second_order = Int.pow 10 (digits' - divisor - divisor) in
    let first_piece = i' / first_order in
    let second_piece = i mod first_order |> fun x -> x / second_order in
    (* Printf.printf "first: %d | second: %d\n" first_piece second_piece; *)
    if first_piece < second_piece then first_piece
    else if first_piece > second_piece then first_piece - 1
    else if first_piece = second_piece && digits' = divisor * 2 then first_piece
    else if first_piece = second_piece && digits' > divisor * 2 then
      (get_upper_bound_2' [@tailcall]) (i mod first_order) (digits' - divisor)
    else failwith "Unhandled case in get_upper_bound_2"
  in
  get_upper_bound_2' i digits

(*
let _ = get_upper_bound_2 ~i:824824821 ~digits:9 ~divisor:3
let _ = get_upper_bound_2 ~i:824824824 ~digits:9 ~divisor:3
let _ = get_upper_bound_2 ~i:824824936 ~digits:9 ~divisor:3
*)

let get_wrong_codes (input_pairs: input_pair list) =
  let open List in
  let get_codes input_pair =
    let {lower; upper; digits} = input_pair in
    let divisors = get_divisors digits in
    map divisors ~f:(fun divisor ->
        let start = get_lower_bound_2 ~i:lower ~digits ~divisor in
        let stop = get_upper_bound_2 ~i:upper ~digits ~divisor in
        (* Printf.printf "start: %d | stop: %d | divisor: %d\n" start stop divisor; *)
        if stop < start then None
        else
          let pieces = (Int.range start stop |> of_iter) in
          let num_pieces = (digits / divisor) in
          Some (map pieces ~f:(fun i -> format_number num_pieces i)))

  in
  map input_pairs ~f:get_codes
  (* TODO: optimize this part *)
  |> map ~f:(filter_map ~f:Fun.id)
  |> flatten |> flatten
  (* --- *)
  |> sort_uniq ~cmp:Int.compare
  |> fold_left ~init:0 ~f:(+)

(*
let _ = get_wrong_codes [{lower = 998; upper = 999; digits = 3};
                         {lower = 1000; upper = 1012; digits = 4}]
*)

let get_part_2 input =
  get_input_pairs input
  |> List.map ~f:get_wrong_codes
  |> List.fold_left ~init:0 ~f:(+)

(*

let _ = [{start = 11; stop = 22222}] |> get_part_2

let _ = parse_input "../inputs/day02.sample.txt" |> get_part_2

let _ = parse_input "../inputs/day02.txt" |> get_part_2

*)
