(*
 * The upper boundary of a variable describes what can be passed in
 * as a value, whereas the lower boundary tells what does the value
 * satisfy if it is retrieved.
 *)

module Base = struct
  type 'a upper_boundary = Top | Constrained of 'a
  type 'a lower_boundary = Bottom | Constrained of 'a

  type 'a t = {
    (* What can be passed in as a value (+) *)
    as_input: 'a upper_boundary;
    (* What is satisfied if retrieved (-) *)
    as_output: 'a lower_boundary;
  }
end

(* |u| Requires that t implements at least c (union) *)
(* |n| Requires that t implements only c (intersection) *)

module Ruby = struct
  type typ =
    | THash
    | TBool
    | TFloat
    | TInt
    | TArray of typ
    | TNil
    | TString
    | TSymbol

  type constraint_t =
    | Binding
    | Literal of typ
    | FunctionApplication
    | Equality
    | Disjuction
    | Overload
    | Class
    | SubType

  type t = (constraint_t list) Base.t
end

include Base

let free_type = { as_input = Top; as_output = Bottom }

(* Appends a constraint to a specific polarity of the provided type *)
(* (+x, -x) -> c -> t where -x <= s *)
let input_constraint
  (x : Ruby.t)
  (c : Ruby.constraint_t) =
  match x.as_input with
  | Top -> { x with as_input = Constrained([c]) }
  | Constrained (s) -> { x with as_input = Constrained(c :: s) }

let output_constraint
  (x : Ruby.t)
  (c : Ruby.constraint_t) =
  match x.as_output with
  | Bottom -> { x with as_output = Constrained([c]) }
  | Constrained (s) -> { x with as_output = Constrained(c :: s) }
