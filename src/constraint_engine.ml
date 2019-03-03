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
  let constraint_mapper lst = function
    (* type variable is bound to a literal *)
    | Literal(ref, t) ->
      let base_reference = base_type_reference t in
      unify_types ref base_reference;
      lst
    (* type variable is a subtype of another *)
    (*| SubType(_, t2) as st ->
      let top = TypeTree.find t2 in
      if top != t2
      then st :: lst (* Keep constraint *)
      else lst (* else drop constraint if no type dependencies *)
      *)
    | _ as cst -> cst :: lst
  in let (_eliminated_types, constrained_types) = constraint_map
    |> ConstraintMap.mapi(
      fun
        (_key : string)
        (constraint_lst : constraint_t list) ->
        constraint_lst |> List.fold_left constraint_mapper []
    )
    |> ConstraintMap.partition (fun _k v -> (List.compare_length_with v 0) = 0)
  in constrained_types

type constraint_category =
  | ReferenceConstraint of string
  | AssignmentConstraint of string * expr_metadata expression
  | OtherConstraint of expr_metadata expr

let map_to_constraint_category e = match e with
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

let rec build_constraints constraint_map (expr, { type_reference; level; _ }) =
  let rec build_constraint type_key = function

    | ReferenceConstraint (name) ->
      let maybe_t = reference_table |> find_or_insert name type_reference in
      begin match maybe_t with
        | Some(t) -> constraint_map |> append_constraint type_key (SubType (t, type_reference))
        | None -> constraint_map
      end

    | AssignmentConstraint (name, iexpr) ->
      begin match typeof_expr expr with
        | GeneralizedType _ -> constraint_map
        | SpecializedType (typ) ->
          let constraint_map = build_constraint type_key (ReferenceConstraint name) in
          let constraint_map = build_constraints constraint_map iexpr in
          begin match typ.elem with
            | TPoly t ->
              append_constraint t (SubType (typ, type_reference)) constraint_map
            | _ -> constraint_map
          end
      end

    | OtherConstraint (e) -> begin match e with
      (* Direct Ruby literal reference *)
      | ExprValue(v) ->
        constraint_map |> append_constraint type_key (Literal (type_reference, typeof_value v))
      (* Method invocation *)
      | ExprCall (receiver_expression, meth, args) ->
        let (iexpr, {type_reference = receiver; _}) = receiver_expression in
        let arg_types = args |> List.map (fun (_, {type_reference; _}) -> type_reference) in
        let constraint_map = match iexpr with
          | ExprVar (name, _) | ExprIVar (name, _) | ExprConst ((name, _), _) ->
            let ref_name = String.concat "" [name; "#"; meth] in
            build_constraint type_key (ReferenceConstraint ref_name)
          | _ -> constraint_map in
        let constraint_map = build_constraints constraint_map receiver_expression
                            |> append_constraint type_key (FunctionApplication(meth, arg_types, receiver)) in
        List.fold_left build_constraints constraint_map args
      (* Lambda definition *)
      | ExprLambda (_, expression) ->
        let (_, {type_reference = return_type_node; _}) = expression in
        let return_t = (TypeTree.find return_type_node).elem in
        let typ = TLambda([TAny], return_t) in
        build_constraints constraint_map expression
        |> append_constraint type_key (Literal(type_reference, typ))
      | _ -> constraint_map
    end
  in match (level, type_reference.elem) with
  | 0, TPoly (type_key) -> build_constraint type_key (map_to_constraint_category expr)
  | _ -> constraint_map
