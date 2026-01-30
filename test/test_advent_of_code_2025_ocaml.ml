let () =
  let open Alcotest in
  run "AOC days" [
    "Day02", Test_day02.suite
  ]
