open OUnit2
open Ruby_tie
open RubyTypes

let type_cache = ConstraintEngine.base_type_cache ()

let gen_type elt =
  let default_metadata = { location = None; binding = None; level = Unresolved } in
  Util.Disjoint_set.make ~root:false ~metadata:default_metadata elt

let test_solve _ =
  let t1 = (gen_type @@ TPoly "T1") in
  let t2 = (gen_type @@ TPoly "T2") in
  let t3 = (gen_type @@ TPoly "T3") in

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

let suite = "Constraint Engine Tests" >::: [
  "solve" >:: test_solve;
]
