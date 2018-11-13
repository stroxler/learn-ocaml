open OUnit2

let suite =
    "All" >::: [
        "Arith" >::: Test_arith.tests;
    ]

let () = run_test_tt_main suite
