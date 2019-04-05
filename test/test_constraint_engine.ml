open OUnit2
open Ruby_tie
open RubyTypes

let type_cache = ConstraintEngine.base_type_cache ()

let gen_type elt ~binding =
  let default_metadata = { location = None; binding = binding; level = Unresolved } in
  Util.Disjoint_set.make ~root:false ~metadata:default_metadata elt

let test_simplify _ =
  todo "Not sure what the expected result is";

  let t1 = gen_type ~binding:(Some "t1") (TPoly "T1") in
  let t2 = gen_type ~binding:None (TPoly "T2") in

  let open ConstraintEngine.Constraints in
  let constraints = Map.empty
  |> Map.add_seq @@ List.to_seq [
    ("T1", [ SubType(t1, t2); ]);
    ("T2", [ Literal(t2, TBool); ]);
  ]
  |> ConstraintEngine.simplify in

  let open Util.Disjoint_set in
  assert_equal [ SubType(t1, t2) ] (Map.find "T1" constraints);
  assert_equal TBool (find t2).elem;
  assert_equal false (Map.mem "T2" constraints)

let test_solve _ =
  let t1 = gen_type ~binding:None (TPoly "T1") in
  let t2 = gen_type ~binding:None (TPoly "T2") in
  let t3 = gen_type ~binding:None (TPoly "T3") in

  let bool_t = type_cache TBool in
  let int_t = type_cache TInt in

  let open ConstraintEngine.Constraints in
  let constraints = Map.empty |> Map.add_seq @@ List.to_seq [
    ("T1", [ SubType(t1, t2); SubType(t1, t3) ]);
    ("T2", [ SubType(t2, bool_t) ]);
    ("T3", [ SubType(t3, int_t) ]);
  ] in

  let _ = ConstraintEngine.solve constraints in

  let open Util.Disjoint_set in
  assert_equal (TUnion(TInt, TBool)) (find t1).elem;
  assert_equal TBool (find t2).elem;
  assert_equal TInt (find t3).elem

let test_solve_raises_on_impossible_constraints _ =
  let t1 = gen_type ~binding:(Some "a") (TPoly "T1") in

  let open ConstraintEngine.Constraints in
  let constraints = Map.empty
  |> Map.add "T1"  [ Literal(t1, TBool); Literal(t1, TInt) ] in

  let assertion = fun () -> (ConstraintEngine.simplify constraints) in
  assert_raises (Util.Disjoint_set.Incompatible_nodes) assertion

let suite = "Constraint Engine Tests" >::: [
  "simplify" >:: test_simplify;
  "solve" >:: test_solve;
  "solve with impossible constraints" >:: test_solve_raises_on_impossible_constraints;
]
