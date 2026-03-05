open ContainersLabels

let parse_input path =
  IO.with_in path @@ fun ic ->
  IO.read_lines_l ic
  |> List.map ~f:(fun s -> String.to_list s
                   |> List.map ~f:Char.to_string
                   |> List.map ~f:Int.of_string_exn)

(*
let sample_input = parse_input "../inputs/day03.sample.txt"
let input = parse_input "../inputs/day03.txt"
*)

type input = int list list

(* Part 1 *)

let solve_part_1 (input: input) (* : int *) =
  let get_first_values = function
    | [] | [_] | [_; _] -> failwith "Input must have at least three joltages"
    | a :: b :: rest -> (a, b), rest
  in
  let process_line acc c =
    let a, b = acc in
    let list = List.sort
        [((a, b), a * 10 + b);
         ((b, c), b * 10 + c);
         ((a, c), a * 10 + c)]
        ~cmp:(fun x y ->
            let _, n = x in
            let _', n' = y in
            Int.compare n' n)
    in
    let acc', _ = List.get_at_idx_exn 0 list in
    acc'
  in
  List.map input
    ~f:(fun line ->
        let acc, coll = get_first_values line in
        List.fold_left coll ~init:acc ~f:process_line)
  |> List.map ~f:(fun (x, y) -> x * 10 + y)
  |> List.reduce_exn ~f:(+)

(*
let _ = solve_part_1 sample_input
let _ = solve_part_1 input
*)

(* Part 2 *)

let split_list_at ~i ~list =
  let rec accumulate head cnt tail =
    match tail with
    | [] -> failwith "length shorter than index"
    | x :: tail' when cnt = i -> (List.rev @@ x :: head, tail')
    | x :: tail' -> accumulate (x :: head) (cnt + 1) tail'
  in
  accumulate [] 0 list

(* let _ = split_list_at ~i:3 ~list:(Int.range 10 30 |> List.of_iter) *)

let format_joltage batteries =
  batteries
  |> List.map ~f:Int.to_string
  |> String.concat ~sep:""
  |> Int.of_string_exn

let get_joltage_12 (line: int list) =
  let acc, tail = split_list_at ~i:(12 - 1) ~list:line in
  let keep_best_batteries acc i =
    let acc' = acc @ [i] in
    let indices = Int.range 0 (List.length acc' - 1) |> List.of_iter in
    List.fold_left indices ~init:[]
      ~f:(fun numbers index ->
          let batteries = List.remove_at_idx index acc' in
          let number = format_joltage batteries in
          (batteries, number) :: numbers)
    |> List.sort ~cmp:(fun x y ->
        let _, n = x in
        let _', n' = y in
        Int.compare n' n)
    |> List.get_at_idx_exn 0
    |> (fun (batteries, _) -> batteries)
  in
  List.fold_left tail ~init:acc ~f:keep_best_batteries

(*
let _ = input |> List.get_at_idx_exn 0 |> get_joltage_12

Result
let _ =
  input
  |> List.map ~f:get_joltage_12
  |> List.fold_left ~init:0
    ~f:(fun acc batteries -> format_joltage batteries + acc)
*)
