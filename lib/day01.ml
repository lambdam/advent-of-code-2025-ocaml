open ContainersLabels

type rotation =
  | Right of int
  | Left of int
[@@deriving show]

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


(*
let input_sample = parse_input "../inputs/day01.sample.txt"
let input = parse_input "../inputs/day01.txt"
*)

let inverse_pos pos =
  (100 - pos) mod 100

let identity (x: int) = x

(* Part 1 *)

let solve_part1 (input: rotation list) =
  let rec rotate clicks f tail pos cnt =
    let pos' = (f pos + clicks) mod 100 |> f in
    loop tail pos' (if pos' = 0 then cnt + 1 else cnt)
  and loop input pos cnt =
    match input with
    | [] -> cnt
    | Right clicks :: tail -> rotate clicks identity tail pos cnt
    | Left clicks :: tail -> rotate clicks inverse_pos tail pos cnt
  in
  loop input 50 0

let solve_part1' (input: rotation list) =
  List.fold_left input ~init:(50, 0)
    ~f:(fun (pos, cnt) rot ->
      let pos' = match rot with
        | Right clicks -> (pos + clicks) mod 100
        | Left clicks -> (inverse_pos pos + clicks) mod 100 |> inverse_pos
      in
      pos', (if pos' = 0 then cnt + 1 else cnt))
  |> fun (_, cnt) -> cnt

(*
Send following lines to utop (in Emacs) to get the answers
solve_part1 input_sample
solve_part1' input_sample
solve_part1 input
solve_part1' input
*)

(* Part 2 *)

let show_step pos1 rot pos2 zero_clicks cnt =
  Printf.sprintf "pos: %d, %s, pos': %d, zero_clicks: %d, cnt': %d\n"
    pos1 (show_rotation rot) pos2 zero_clicks cnt

let solve_part2 (input: rotation list) =
  let rotate clicks f pos cnt =
    let sum = (f pos) + clicks in
    let pos' = sum mod 100 |> f in
    let zero_clicks = sum / 100 in
    let cnt' = cnt + zero_clicks in
    pos', cnt'
  in
  (* TODO: reintroduce labelled tuples when ppx_deriving will handle them *)
  List.fold_left input ~init:(50, 0)
    ~f: begin fun (pos, cnt) rot ->
      match rot with
      | Right clicks -> rotate clicks identity pos cnt
      | Left clicks -> rotate clicks inverse_pos pos cnt
    end
  |> fun (_, cnt) -> cnt

(*
Send following lines to utop (in Emacs) to get the answers
solve_part2 input_sample
solve_part2 input
*)
