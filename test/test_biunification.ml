open OUnit2
open Ruby_tie
open Biunification

let test_input_constraint _ =
  let constriant = Ruby.(Literal TInt) in
  let used_as_integer = free_type |> input_constraint constriant in
  let expected_constraint : Ruby.upper_bound = Constrained([constriant]) in
  assert_equal expected_constraint used_as_integer.input_constraints;
  assert_equal Bottom used_as_integer.output_constraints

let test_output_constraint _ =
  let constriant = Ruby.(Literal TInt) in
  let set_as_integer = free_type |> output_constraint constriant in
  let expected_constraint : Ruby.lower_bound = Constrained([constriant]) in
  assert_equal expected_constraint set_as_integer.output_constraints;
  assert_equal Top set_as_integer.input_constraints

(*
  def foo(z)
    x = z
    y = x.incr(1)
    return x
  end
*)

(* let test_subtype _ =
  let open Ruby in
  let z = free_type
  and x = free_type in
  let x' = x
    |> input_constraint (SubType z)
    |> output_constraint (
        Method ("incr", [Constrained([Literal TInt])], Bottom)
      )
  in assert_equal "< .incr : (int -> bottom) >" (solve x) *)

let suite = "Biunification Tests" >::: [
  "input_constraint" >:: test_input_constraint;
  "output_constraint" >:: test_output_constraint;
  (* "subtype" >:: test_subtype; *)
]
