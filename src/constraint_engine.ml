open Types
open Ast

exception TypeError of type_reference * type_reference
(* exception AssignmentError of expression * expression *)

type constraint_t =
  | Binding of string * type_reference
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
    let union_t_node = TypeTree.make ~root:true a.metadata union_t in
    replace_root a union_t_node

let unify_types_exn a b = TypeTree.union a b

let append_constraint k c map =
  let lst = match ConstraintMap.find_opt k map with
    | Some(lst) -> lst
    | None -> []
  in map |> ConstraintMap.add k (c :: lst)

let find_or_insert name t tbl =
  if Hashtbl.mem tbl name
  then begin
    Printf.printf "Found existing type for %s\n" name;
    unify_types t (Hashtbl.find tbl name) (* TODO this should not unify, it should create subtype *)
  end
  else Hashtbl.add tbl name t

let find_or_insert2 name t tbl =
  if Hashtbl.mem tbl name
  then Some(Hashtbl.find tbl name)
  else (Hashtbl.add tbl name t; None)

let rec build_constraints constraint_map (expr, { type_reference; level }) =
  let build_constraint type_key = function
    | ExprVar(name, _)
    | ExprIVar(name, _)
    | ExprConst((name, _), _) ->
      (* reference_table |> find_or_insert name type_reference; *)
      let maybe_t = reference_table |> find_or_insert2 name type_reference in
      begin match maybe_t with
        | Some(t) -> constraint_map |> append_constraint type_key (SubType (t, type_reference))
        | None -> constraint_map
      end
    | ExprValue(v) ->
      constraint_map
      |> append_constraint type_key (Literal (type_reference, typeof_value v))
    | ExprAssign (name, ((_, metadata) as iexpr))
    | ExprIVarAssign (name, ((_, metadata) as iexpr))
    | ExprConstAssign (name, ((_, metadata) as iexpr)) ->
      begin match typeof_expr expr with
        | RawType _ -> constraint_map
        | TypeMetadata (metadata) ->
          let typ = metadata.type_reference in
          (* reference_table |> find_or_insert name typ; *)
          let maybe_t = reference_table |> find_or_insert2 name type_reference in
          let constraint_map = match maybe_t with
            | Some(t) -> constraint_map |> append_constraint type_key (SubType (t, type_reference))
            | None -> constraint_map
          in
          let constraint_map = build_constraints constraint_map iexpr
                               |> append_constraint type_key (Binding (name, type_reference)) in
          begin match metadata.type_reference.elem with
            | TPoly t ->
              append_constraint t (SubType (typ, type_reference)) constraint_map
            | _ -> constraint_map
          end
      end
    | ExprCall (receiver_expression, meth, args) ->
      let (iexpr, {type_reference = receiver}) = receiver_expression in
      let () = match iexpr with
        | ExprVar (name, _) | ExprIVar (name, _) | ExprConst ((name, _), _) ->
          let ref_name = String.concat "" [name; "#"; meth] in
          reference_table |> find_or_insert ref_name type_reference
        | _ -> () in
      let arg_types = args |> List.map (fun (_, {type_reference}) -> type_reference) in
      let constraint_map = build_constraints constraint_map receiver_expression
                           |> append_constraint type_key (FunctionApplication(meth, arg_types, receiver)) in
      List.fold_left build_constraints constraint_map args
    | ExprLambda (_, expression) ->
      let (_, {type_reference = return_type_node}) = expression in
      let return_t = (TypeTree.find return_type_node).elem in
      let typ = TLambda([TAny], return_t) in
      build_constraints constraint_map expression
      |> append_constraint type_key (Literal(type_reference, typ))
    | _ -> constraint_map
  in match (level, type_reference.elem) with
  | 0, TPoly (type_key) -> build_constraint type_key expr
  | _ -> constraint_map
