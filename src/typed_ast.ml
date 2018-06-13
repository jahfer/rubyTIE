open Ast

(* Define data structures and types for AST *)

type t =
  | THash
  | TBool
  | TFloat
  | TInt
  | TArray of t
  | TNil
  | TString
  | TSymbol
  | TConst of t
  | TAny
  | TPoly of string
  | TLambda of t list * t
  | TFunc of t list * t

module TypeTree = Disjoint_set

type metadata = {
  expr_loc : Location.t;
  type_reference : t TypeTree.t;
  level : int;
}

let rec type_to_str = function
  | THash    -> "hash"
  | TString  -> "string"
  | TSymbol  -> "symbol"
  | TInt     -> "int"
  | TFloat   -> "float"
  | TBool    -> "bool"
  | TNil     -> "nil"
  | TAny     -> "any"
  | TConst t ->
    Core.sprintf "const<%s>" (type_to_str t)
  | TArray t ->
    Core.sprintf "array<%s>" (type_to_str t)
  | TLambda (args, ret) -> 
    Core.sprintf "lambda<args -> %s>" (type_to_str ret)
  | TFunc (args, ret) ->
    Core.sprintf "func<args -> %s>" (type_to_str ret)
  | TPoly t ->
    Core.sprintf "%s" t

(* Build map and generator for unique type names *)

let current_var = ref 1
let gen_fresh_t () =
  let tv = !current_var in incr current_var;
  (* Printf.printf "-- Creating new type var t/1%03i\n" tv; *)
  TPoly(Core.sprintf "T%i" tv)

(* Annotations *)

let annotate expression =
  let rec annotate_expression expr location_meta =
    let t = gen_fresh_t () in
    let t_nodeT = TypeTree.make t in
    (expr, { expr_loc = location_meta; type_reference = t_nodeT; level = 0 })
  in let (expr, location_meta) = expression in
  replace_metadata annotate_expression expr location_meta

(* Constraint Generation *)

(* TODO: One record per subtype *)
let type_tree_node =
  let 
  t_hash = TypeTree.make THash and
  t_bool = TypeTree.make TBool and
  t_float = TypeTree.make TFloat and
  t_int = TypeTree.make TInt and
  t_array = TypeTree.make @@ TArray (TAny) and
  t_nil = TypeTree.make TNil and
  t_string = TypeTree.make TString and
  t_symbol = TypeTree.make TSymbol and
  t_const = TypeTree.make @@ TConst (TAny) and
  t_any = TypeTree.make TAny and
  t_poly = TypeTree.make @@ TPoly ("x") and
  (* TODO bind on types, not generics! *)
  t_lambda = TypeTree.make @@ TLambda ([TAny], TAny) and
  t_func = TypeTree.make @@ TFunc ([TAny], TAny) in
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

let rec typeof_value = function
  | Hash _   -> THash
  | Bool _   -> TBool
  | Float _  -> TFloat
  | Int _    -> TInt
  | Array _  -> TArray (gen_fresh_t ())
  | String _ -> TString
  | Symbol _ -> TSymbol
  | Nil      -> TNil
  | Any      -> gen_fresh_t ()

let rec typeof_expr = function
  | ExprVar ((_, value))
  | ExprIVar ((_, value))
  | ExprConst ((_, value), _)
  | ExprValue (value) -> type_tree_node @@ typeof_value value
  | ExprFunc (_, _, (_, metadata))
  | ExprLambda (_, (_, metadata))
  | ExprBlock (_, (_, metadata))
  | ExprAssign (_, (_, metadata))
  | ExprIVarAssign (_, (_, metadata))
  | ExprCall ((_, metadata), _, _)
  | ExprConstAssign (_, (_, metadata)) ->
    let { type_reference } = metadata in type_reference

(* TODO: Reference `t TypeTree.t` instead of naked `t` *)
type constraint_t =
  | Binding of string * t TypeTree.t
  | Literal of t TypeTree.t
  | FunctionApplication of string * t TypeTree.t list * t TypeTree.t (* member name, args, return value *)
  | Equality of t TypeTree.t * t TypeTree.t
  | Disjuction of constraint_t list
  | Overload of t TypeTree.t
  | Class of t TypeTree.t

module ConstraintMap = Map.Make (String)

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
      let typ = typeof_expr expr in 
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

(* AST -> TypedAST *)
let apply_constraints ast constraint_map =
  let annotate_expression expr ({ type_reference } as meta) =
    (expr, { meta with type_reference = TypeTree.find type_reference })
  in let (expr, meta) = ast in
  replace_metadata annotate_expression expr meta

(* Printer Utility *)

module Printer = struct
  open Printf

  let rec print_typed_expr ~indent outc = function
    | ExprCall (receiver, meth, args) ->
      printf "send %a `%s" (print_expression ~indent:(indent+1)) receiver meth;
      (args |> List.iteri (fun i expr -> print_expression ~indent:(indent+2) outc expr))
    | ExprFunc (name, args, body) ->
      printf "def `%s %a %a" name Ast.Printer.print_args args (print_expression ~indent:(indent+1)) body
    | ExprLambda (args, body) ->
      printf "lambda %a %a" Ast.Printer.print_args args (print_expression ~indent:(indent+1)) body
    | ExprVar ((name, value))  ->
      printf "lvar `%s" name
    | ExprConst ((name, value), base) ->
      printf "const %a `%s" (print_expression ~indent:(indent+1)) base name
    | ExprIVar ((name, value)) ->
      printf "ivar `%s" name
    | ExprAssign (name, expr) ->
      printf "lvasgn `%s %a" name (print_expression ~indent:(indent+1)) expr
    | ExprIVarAssign (name, expr) ->
      printf "ivasgn %s %a" name (print_expression ~indent:(indent+1)) expr
    | ExprConstAssign (name, expr) ->
      printf "casgn %s %a" name (print_expression ~indent:(indent+1)) expr
    | ExprValue (value) ->
      printf "%a" Ast.Printer.print_value value
    | ExprBlock (expr1, expr2) ->
      printf "%a %a" (print_expression ~indent:(indent+1)) expr1 (print_expression ~indent:(indent+1)) expr2

  and print_expression ~indent outc (expr, metadata) =
    if (indent <> 1) then printf "\n";
    let { expr_loc; type_reference; level } = metadata in
    (* printf "# %a\n" Location.print_loc expr_loc; *)
    printf "%*s(%s : %a)" indent " "
      (type_to_str type_reference.elem)
      (print_typed_expr ~indent:indent)
      expr;
    if (indent = 1) then printf "\n"

  let print_constraint k v =
    match v with
    | FunctionApplication (member, args, receiver_t) ->
      printf "\027[31m[CONSTRAINT: FunctionApplication ((%s) -> %s =Fn { %s.%s })]\027[m\n"
        (if List.length args > 0 then 
          (String.concat ", " (List.map (fun arg ->
            type_to_str (TypeTree.find arg).elem) args))
        else "")
        k (type_to_str (TypeTree.find receiver_t).elem) member
    | Binding (name, t) ->
      printf "\027[31m[CONSTRAINT: Binding (%s = %s)]\027[m\n" name (type_to_str t.elem)
    | Literal _ | Equality _ -> ()
    (* | Literal (t) ->
      printf "\027[31m[CONSTRAINT: Literal (%s = %s)]\027[m\n" k (type_to_str t.elem)
    | Equality (a, b) ->
      printf "\027[31m[CONSTRAINT: Equality (%s = %s)]\027[m\n" (type_to_str a.elem) (type_to_str b.elem) *)
    | _ ->
      printf "\027[31m[CONSTRAINT: %s => Unknown]\027[m\n" k

  let print_constraint_map constraint_map =
    constraint_map |> ConstraintMap.iter (fun k vs ->
      vs |> List.iter (fun v -> print_constraint k v)
    )

  let debug_union (a : t TypeTree.t) (b : t TypeTree.t) =
    Printf.printf "-- Unifying %s and %s\n" (type_to_str a.elem) (type_to_str b.elem)
end

let rec to_typed_ast core_expr =
  let constraint_map = ConstraintMap.empty in
  let annotations = annotate core_expr in
  let constraints = build_constraints constraint_map annotations in
  let _ = Printer.print_constraint_map constraints in
  apply_constraints annotations constraints
