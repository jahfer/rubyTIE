open OUnit2
open Ruby_tie
open RubyTypes

let type_cache = ConstraintEngine.base_type_cache ()

let gen_type elt =
  let default_metadata = { location = None; binding = None; level = Unresolved } in
  Util.Disjoint_set.make ~root:false ~metadata:default_metadata elt

let test_unify_constraints _ =
  let t1 = (gen_type @@ TPoly "T1") in
  let t2 = (gen_type @@ TPoly "T2") in
  let t3 = (gen_type @@ TPoly "T3") in

  let bool_t = type_cache TBool in
  let int_t = type_cache TInt in

  let _ = ConstraintEngine.unify_constraints [
    (t1, [t2; t3]); (t2, [bool_t]); (t3, [int_t]);
  ] in

  let open Util.Disjoint_set in
  let t1' = find t1 in
  let t2' = find t2 in
  let t3' = find t3 in

  assert_equal (TUnion(TBool, TInt)) t1'.elem;
  assert_equal TBool t2'.elem;
  assert_equal TInt t3'.elem

let suite = "Constraint Engine Tests" >::: [
  "unify_constraints" >:: test_unify_constraints;
]
