open Types
open Ast

exception TypeError of type_reference * type_reference
(* exception AssignmentError of expression * expression *)

type constraint_t =
  | Literal of type_reference * Types.t
  | FunctionApplication of string * type_reference list * type_reference (* member name, args, return value *)
  | Equality of type_reference * type_reference
  | Disjuction of constraint_t list
  | Overload of type_reference
  | Class of type_reference
  | SubType of type_reference * type_reference

module ConstraintMap = Map.Make (String)

let reference_table : (string, type_reference) Hashtbl.t = Hashtbl.create 1000

let unify_types a b =
  let open TypeTree in
  Printf.printf "-- %s &\n-- %s\n\n" (Printer.print_inheritance a) (Printer.print_inheritance b);
  try union a b with Incompatible_nodes ->
    let union_t = TUnion((find a).elem, (find b).elem) in
    let union_t_node = TypeTree.make ~root:true ~metadata:a.metadata union_t in
    replace_root a union_t_node

let unify_types_exn a b = TypeTree.union a b

let append_constraint k c map =
  let lst = match ConstraintMap.find_opt k map with
    | Some(lst) -> lst
    | None -> []
  in map |> ConstraintMap.add k (c :: lst)

let find_or_insert name t tbl =
  if Hashtbl.mem tbl name
  then Some(Hashtbl.find tbl name)
  else (Hashtbl.add tbl name t; None)

(*
  After constraints have been found, iterate over constraints
  until definition for all types can be found.
*)
let simplify constraint_map =
  let step_1 lst = function
    (* type variable is bound to a literal *)
    | Literal(ref, t) ->
      let base_reference = base_type_reference t in
      unify_types ref base_reference;
      lst
    | _ as cst -> cst :: lst
  in
  let step_2 lst = function
    | SubType(subtype, supertype) as st ->
      if (TypeTree.find subtype).metadata.level = Resolved ||
        Util.is_some subtype.metadata.binding
      then st :: lst
      else (subtype.parent := supertype; lst)
    | _ as cst -> cst :: lst
  in
  let (_eliminated_types, constrained_types) = constraint_map
    |> ConstraintMap.mapi(
      fun
        (_key : string)
        (constraint_lst : constraint_t list) ->
          constraint_lst
          |> List.fold_left step_1 []
          |> List.fold_left step_2 []
    )
    |> ConstraintMap.partition (
      fun _k v -> (List.compare_length_with v 0) = 0
    )
  in constrained_types

type constraint_category =
  | ReferenceConstraint of string
  | AssignmentConstraint of string * expr_metadata expression
  | OtherConstraint of expr_metadata expr

let expr_to_constraint_category e = match e with
  | ExprVar(name, _)
  | ExprIVar(name, _)
  | ExprConst((name, _), _) -> ReferenceConstraint name
  | ExprAssign (name, expr)
  | ExprIVarAssign (name, expr)
  | ExprConstAssign (name, expr) -> AssignmentConstraint (name, expr)
  | ExprCall _
  | ExprLambda _
  | ExprValue _
  | ExprFunc _
  | ExprBlock _ -> OtherConstraint e

let rec build_constraints constraint_map (expr, { type_reference; _ }) =

  let rec build_constraint type_var_name = function

    | ReferenceConstraint (name) ->
      let opt_existing_type = reference_table
      |> find_or_insert name type_reference in

      begin match opt_existing_type with
        | Some (existing_type) ->
          constraint_map
          |> append_constraint type_var_name @@ SubType (existing_type, type_reference)
        | None -> constraint_map
      end

    | AssignmentConstraint (name, iexpr) ->
      begin match typeof_expr expr with
        | GeneralizedType _ -> constraint_map
        | SpecializedType (expr_type) ->
          let constraint_map =
            ReferenceConstraint name
            |> build_constraint type_var_name
            |> Util.flip build_constraints iexpr in
          match expr_type.elem with
          | TPoly t ->
            constraint_map
            |> append_constraint t @@ SubType (expr_type, type_reference)
          | _ -> constraint_map
      end

    | OtherConstraint (e) -> match e with

      (* Direct Ruby literal reference *)
      | ExprValue (value) ->
        constraint_map
        |> append_constraint type_var_name @@ Literal (type_reference, typeof_value value)

      (* Method invocation *)
      | ExprCall (receiver_expression, meth, args) ->
        let (receiver_expr, {type_reference = receiver_type; _}) = receiver_expression
        and arg_types = args
          |> List.map (fun (_, {type_reference; _}) -> type_reference)
        in begin
          match (expr_to_constraint_category receiver_expr) with
          | ReferenceConstraint (receiver_name) ->
            let method_name = String.concat "" [ receiver_name; "#"; meth ] in
            build_constraint type_var_name @@ ReferenceConstraint method_name
          | _ -> constraint_map
        end
        |> Util.flip build_constraints receiver_expression
        |> append_constraint type_var_name @@ FunctionApplication (meth, arg_types, receiver_type)
        |> Util.flip (List.fold_left build_constraints) args

      (* Lambda definition *)
      | ExprLambda (_, expression) ->
        let (_, {type_reference = return_type_node; _}) = expression in
        let return_type = (TypeTree.find return_type_node).elem in
        let lambda_type = TLambda([TAny], return_type) in
        build_constraints constraint_map expression
        |> append_constraint type_var_name @@ Literal (type_reference, lambda_type)

      | _ -> constraint_map
  in
  match type_reference.elem with
  | TPoly (type_var_name) ->
    expr_to_constraint_category expr
    |> build_constraint type_var_name
  | _ -> constraint_map
