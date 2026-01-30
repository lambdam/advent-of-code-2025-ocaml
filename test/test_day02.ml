module Day02 = Advent_of_code_2025_ocaml.Day02

let suite =
  let open Alcotest in [
    test_case "is_even" `Quick (fun () ->
        (check bool) "2 should be even" true (Day02.is_even 2));
    test_case "¬ is_even" `Quick (fun () ->
        (check bool) "3 should not be even" false (Day02.is_even 3));
    test_case "is_odd" `Quick (fun () ->
        (check bool) "3 should be odd" true (Day02.is_odd 3));
    test_case "¬ is_odd" `Quick (fun () ->
        (check bool) "2 should not be odd" false (Day02.is_odd 2));
    (* Lower bound *)
    test_case "numbers with odd number of digits should have a raised lower bound"
      `Quick (fun () ->
          (check int) "should raise" 10 (Day02.get_lower_bound 111));
    test_case "numbers with odd number of digits should have a raised lower bound"
      `Quick (fun () ->
          (check int) "should raise" 1000 (Day02.get_lower_bound 1234567));
    test_case "numbers with even number of digits and tail > head"
      `Quick (fun () ->
          (check int) "should inc head" 11 (Day02.get_lower_bound 1050));
    test_case "numbers with even number of digits and tail < head"
      `Quick (fun () ->
          (check int) "should be head" 10 (Day02.get_lower_bound 1005));
    test_case "numbers with even number of digits and tail = head"
      `Quick (fun () ->
          (check int) "should be head" 10 (Day02.get_lower_bound 1010));
    (* Upper bound *)
    test_case "numbers with odd number of digits should have a lowered upper bound"
      `Quick (fun () ->
          (check int) "should lower" 9 (Day02.get_upper_bound 111));
    test_case "numbers with odd number of digits should have a lowered upper bound"
      `Quick (fun () ->
          (check int) "should lower" 999 (Day02.get_upper_bound 1234567));
    test_case "numbers with even number of digits and tail > head"
      `Quick (fun () ->
          (check int) "should be head" 10 (Day02.get_upper_bound 1050));
    test_case "numbers with even number of digits and tail < head"
      `Quick (fun () ->
          (check int) "should dec head" 9 (Day02.get_upper_bound 1005));
    test_case "numbers with even number of digits and tail = head"
      `Quick (fun () ->
          (check int) "should be head" 10 (Day02.get_upper_bound 1010));
  ]
