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
