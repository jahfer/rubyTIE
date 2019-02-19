open OUnit2
open Ruby_tie
open Biunification

let test_input_constraint _ =
  let used_as_integer = input_constraint free_type Ruby.(Literal(TInt)) in
  let expected_constraint : (Ruby.constraint_t list) upper_boundary = Constrained([Ruby.(Literal(TInt))]) in
  assert_equal expected_constraint used_as_integer.as_input;
  assert_equal Bottom used_as_integer.as_output

let test_output_constraint _ =
  let set_as_integer = output_constraint free_type Ruby.(Literal(TInt)) in
  let expected_constraint : (Ruby.constraint_t list) lower_boundary = Constrained([Ruby.(Literal(TInt))]) in
  assert_equal expected_constraint used_as_integer.as_output;
  assert_equal Top used_as_integer.as_input

let suite = "Biunification Tests" >::: [
  "input_constraint" >:: test_input_constraint;
  "output_constraint" >:: test_output_constraint;
]
