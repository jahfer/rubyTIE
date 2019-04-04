open OUnit2

let suite = "RubyTIE" >::: [Test_biunification.suite; Test_constraint_engine.suite]

let _ = run_test_tt_main suite
