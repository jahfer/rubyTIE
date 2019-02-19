open OUnit2

let suite = "RubyTIE" >::: [Test_biunification.suite]

let _ = run_test_tt_main suite
