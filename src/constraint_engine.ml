open Types
open Ast

exception TypeError of type_reference * type_reference

(* TODO bind on types, not generics! *)
let type_tree_node =
  let make_root_node = TypeTree.make ~root:true None in
  let 
    t_hash = make_root_node THash and
    t_bool = make_root_node TBool and
    t_float = make_root_node TFloat and
    t_int = make_root_node TInt and
    t_array = make_root_node (TArray TAny) and
    t_nil = make_root_node TNil and
    t_string = make_root_node TString and
    t_symbol = make_root_node TSymbol and
    t_const = make_root_node (TConst TAny) and
    t_any = make_root_node TAny and
    t_poly = make_root_node (TPoly "x") and
    t_lambda = make_root_node (TLambda ([TAny], TAny)) and
    t_func = make_root_node (TFunc ([TAny], TAny)) in
  function
  | THash -> incr t_hash.rank; t_hash
  | TBool -> incr t_bool.rank; t_bool
  | TFloat -> incr t_float.rank; t_float
  | TInt -> incr t_int.rank; t_int
  | TArray _ -> incr t_array.rank; t_array
  | TNil -> incr t_nil.rank; t_nil
  | TString -> incr t_string.rank; t_string
  | TSymbol -> incr t_symbol.rank; t_symbol
  | TConst _ -> incr t_const.rank; t_const
  | TAny -> incr t_any.rank; t_any
  | TPoly _ -> incr t_poly.rank; t_poly
  | TLambda _ -> incr t_lambda.rank; t_lambda
  | TFunc _ -> incr t_func.rank; t_func

type constraint_t =
  | Binding of string * type_reference
  | Literal of type_reference
  | FunctionApplication of string * type_reference list * type_reference (* member name, args, return value *)
  | Equality of type_reference * type_reference
  | Disjuction of constraint_t list
  | Overload of type_reference
  | Class of type_reference

module ConstraintMap = Map.Make (String)

let reference_table : (string, type_reference) Hashtbl.t = Hashtbl.create 1000 

let unify_types a b =
  let open TypeTree in
  try (union a b) with
    Incompatible_nodes -> raise (TypeError (a, b))

let append_constraint k c map =
  let lst = match ConstraintMap.find_opt k map with
    | Some(lst) -> lst
    | None -> []
  in map |> ConstraintMap.add k (c :: lst)

let rec build_constraints constraint_map (expr, { type_reference; level }) =
  let build_constraint type_key = function
    | ExprVar(name, v) ->
      if Hashtbl.mem reference_table name
      then unify_types type_reference (Hashtbl.find reference_table name)
      else Hashtbl.add reference_table name type_reference;
      constraint_map
    | ExprValue(v) ->
      unify_types (type_tree_node @@ typeof_value v) type_reference;
      constraint_map
      |> append_constraint type_key (Literal (type_tree_node @@ typeof_value v))
    | ExprAssign (name, ((_, metadata) as iexpr))
    | ExprIVarAssign (name, ((_, metadata) as iexpr))
    | ExprConstAssign (name, ((_, metadata) as iexpr)) ->
      unify_types type_reference metadata.type_reference;
      let typ = match typeof_expr expr with
        | RawType t -> type_tree_node t
        | TypeMetadata (metadata) -> metadata.type_reference
      in let () = match Hashtbl.mem reference_table name with
          | true -> unify_types typ (Hashtbl.find reference_table name)
          | false -> Hashtbl.add reference_table name typ
      in let constraint_map = build_constraints constraint_map iexpr
                              |> append_constraint type_key (Binding (name, type_reference)) in
      begin match typ.elem with
        | TPoly t ->
          unify_types typ type_reference;
          append_constraint t (Equality (typ, type_reference)) constraint_map
        | _ -> (* Never reached *)
          append_constraint type_key (Literal typ) constraint_map
      end
    | ExprCall (receiver_expression, meth, args) ->
      let (_, {type_reference = receiver}) = receiver_expression in
      let arg_types = args |> List.map (fun (_, {type_reference}) -> type_reference) in
      let constraint_map = build_constraints constraint_map receiver_expression
                           |> append_constraint type_key (FunctionApplication(meth, arg_types, receiver)) in
      List.fold_left build_constraints constraint_map args
    | ExprLambda (_, expression) ->
      let (_, {type_reference = return_type_node}) = expression in
      let return_t = (TypeTree.find return_type_node).elem in
      let typ = type_tree_node @@ TLambda([TAny], return_t) in
      let _ = unify_types typ type_reference in
      build_constraints constraint_map expression
      |> append_constraint type_key (Literal(typ))
    | _ -> constraint_map
  in match (level, type_reference.elem) with
  | 0, TPoly (type_key) -> build_constraint type_key expr
  | _ -> constraint_map
