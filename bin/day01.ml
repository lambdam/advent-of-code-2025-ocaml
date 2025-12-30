open Containers

type rotation =
  | Right of int
  | Left of int

let input =
  IO.with_in "../inputs/day01.txt" @@ fun ic ->
  IO.read_lines_seq ic
  |> Seq.map (fun s ->
      let s = String.trim s in
      match String.(sub s 0 1, drop 1 s) with
      | "R", s -> Right (Int.of_string_exn s)
      | "L", s -> Left (Int.of_string_exn s)
      | _ -> failwith "Bad input")
  |> Seq.to_list
