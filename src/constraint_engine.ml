open Types
open Ast

(* TODO bind on types, not generics! *)
let type_tree_node =
  let 
    t_hash = TypeTree.make THash ~root:true and
  t_bool = TypeTree.make TBool ~root:true and
  t_float = TypeTree.make TFloat ~root:true and
  t_int = TypeTree.make TInt ~root:true and
  t_array = TypeTree.make (TArray TAny) ~root:true and
  t_nil = TypeTree.make TNil ~root:true and
  t_string = TypeTree.make TString ~root:true and
  t_symbol = TypeTree.make TSymbol ~root:true and
  t_const = TypeTree.make (TConst TAny) ~root:true and
  t_any = TypeTree.make TAny ~root:true and
  t_poly = TypeTree.make (TPoly "x") ~root:true and
  t_lambda = TypeTree.make (TLambda ([TAny], TAny)) ~root:true and
  t_func = TypeTree.make (TFunc ([TAny], TAny)) ~root:true in
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
  | Binding of string * Types.t TypeTree.t
  | Literal of Types.t TypeTree.t
  | FunctionApplication of string * Types.t TypeTree.t list * Types.t TypeTree.t (* member name, args, return value *)
  | Equality of Types.t TypeTree.t * Types.t TypeTree.t
  | Disjuction of constraint_t list
  | Overload of Types.t TypeTree.t
  | Class of Types.t TypeTree.t

module ConstraintMap = Map.Make (String)
module ReferenceMap = Map.Make (String)

let append_constraint k c map =
  let lst = match ConstraintMap.find_opt k map with
    | Some(lst) -> lst
    | None -> []
  in map |> ConstraintMap.add k (c :: lst)

let rec build_constraints constraint_map (expr, { type_reference; level }) =
  let build_constraint type_key = function
    | ExprValue(v) ->
      TypeTree.union (type_tree_node @@ typeof_value v) type_reference;
      constraint_map
      |> append_constraint type_key (Literal (type_tree_node @@ typeof_value v))
    | ExprAssign (v, ((_, metadata) as iexpr))
    | ExprIVarAssign (v, ((_, metadata) as iexpr))
    | ExprConstAssign (v, ((_, metadata) as iexpr)) ->
      TypeTree.union type_reference metadata.type_reference;
      let typ = match typeof_expr expr with
        | RawType t -> type_tree_node t
        | TypeMetadata (metadata) -> metadata.type_reference
      in 
      let constraint_map = build_constraints constraint_map iexpr
                           |> append_constraint type_key (Binding (v, type_reference)) in
      begin match typ.elem with
        | TPoly t ->
          append_constraint t (Equality (typ, type_reference)) constraint_map
        (* | TLambda (_, (TPoly(t) as poly_t)) ->
            constraint_map
            |> append_constraint t (Equality ((type_tree_node poly_t), type_reference))
            |> append_constraint type_key (Literal typ) *)
        | _ ->
          (* Never reached *)
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
      let _ = TypeTree.union typ type_reference in
      build_constraints constraint_map expression
      |> append_constraint type_key (Literal(typ))
    | _ -> constraint_map
  in match (level, type_reference.elem) with
  | 0, TPoly (type_key) -> build_constraint type_key expr
  | _ -> constraint_map
