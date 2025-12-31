open ContainersLabels

type rotation =
  | Right of int
  | Left of int

let parse_input path =
  IO.with_in path @@ fun ic ->
  IO.read_lines_seq ic
  |> Seq.map (fun s ->
      let s = String.trim s in
      match String.(sub s ~pos:0 ~len:1, drop 1 s) with
      | "R", s -> Right (Int.of_string_exn s)
      | "L", s -> Left (Int.of_string_exn s)
      | _ -> failwith "Bad input")
  |> Seq.to_list

let input_sample_part_1 = parse_input "../inputs/day01.sample.part1.txt"

let input_part_1 = parse_input "../inputs/day01.txt"

let solve_part1 (input: rotation list) =
  let rec rotate clicks tail pos cnt =
    let pos' = (pos + clicks) mod 100 in
    let pos'' = if pos' < 0 then 100 + pos' else pos' in
    loop tail pos'' (if pos'' = 0 then cnt + 1 else cnt)
  and loop input pos cnt =
    match input with
    | [] -> cnt
    | Right clicks :: tail -> rotate clicks tail pos cnt
    | Left clicks :: tail -> rotate (-clicks) tail pos cnt
  in
  loop input 50 0

let solve_part1' (input: rotation list) =
  List.fold_left input ~init:(~pos:50, ~cnt:0)
    ~f:(fun (~pos, ~cnt) rot ->
      let pos' = match rot with
        | Right clicks -> (pos + clicks) mod 100
        | Left clicks ->
          let pos'' = (pos - clicks) mod 100 in
          if pos'' < 0 then 100 + pos'' else pos''
      in
      ~pos: pos', ~cnt: (if pos' = 0 then cnt + 1 else cnt))
|> fun (~cnt, ..) -> cnt

(* let result = solve_part1 input_sample_part_1 *)
(* let result = solve_part1' input_sample_part_1 *)

(* let result = solve_part1 input_part_1 *)
(* let result = solve_part1' input_part_1 *)
