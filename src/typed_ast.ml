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

type metadata = {
  expr_loc : Location.t;
  expr_type : t;
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
  | ExprValue (value) -> typeof_value value
  | ExprFunc (_, _, (_, metadata))
  | ExprLambda (_, (_, metadata))
  | ExprBlock (_, (_, metadata))
  | ExprAssign (_, (_, metadata))
  | ExprIVarAssign (_, (_, metadata))
  | ExprCall ((_, metadata), _, _)
  | ExprConstAssign (_, (_, metadata)) ->
    let { expr_type } = metadata in expr_type

(* Annotations *)

let annotate expression =
  let rec annotate_expression expr location_meta =
    let t = gen_fresh_t () in
    (expr, { expr_loc = location_meta; expr_type = t; level = 0 })
  in let (expr, location_meta) = expression in
  replace_metadata annotate_expression expr location_meta

(* module AnnotationMap = Map.Make (String)
   let annotations = ref AnnotationMap.empty

   let add_annotation name typ map =
   let typ_name = (Printer.type_to_str typ) in
   Printf.printf "\027[31m[ANNOTATION: Bind ('%s' = %s)]\027[m\n" name typ_name;
   AnnotationMap.add name typ map

   let rec create_annotation name =
   match AnnotationMap.find_opt name !annotations with
   | Some(typ) -> typ
   | None -> let gen_typ = gen_fresh_t () in
    annotations := add_annotation name gen_typ !annotations;
    gen_typ *)

(* Constraint Generation *)

type constraint_t =
  | Literal of t
  | FunctionApplication of string * t list * t (* member name, args, return value *)
  | Equality of t * t
  | Disjuction of constraint_t list
  | Overload of t
  | Class of t

module ConstraintMap = Map.Make (String)

let append_constraint k c map =
  let lst = match ConstraintMap.find_opt k map with
    | Some(lst) -> lst
    | None -> []
  in map |> ConstraintMap.add k (c :: lst)

let rec build_constraints constraint_map (expr, { expr_type; level }) =
  let build_constraint type_key = function
    | ExprValue(v) -> 
      constraint_map
      |> append_constraint type_key (Literal (typeof_value v))
    | ExprAssign (v, iexpr)
    | ExprIVarAssign (v, iexpr)
    | ExprConstAssign (v, iexpr) ->
      let constraint_map = build_constraints constraint_map iexpr in
      let typ = typeof_expr expr in begin match typ with
        | TPoly t ->
          append_constraint t (Equality (typ, expr_type)) constraint_map
        | TLambda (_, (TPoly(t) as poly_t)) ->
          constraint_map
          |> append_constraint t (Equality (poly_t, expr_type))
          |> append_constraint type_key (Literal typ)
        | _ -> append_constraint type_key (Literal typ) constraint_map
      end
    | ExprCall (receiver_expression, meth, args) ->
      let (_, {expr_type = receiver_t}) = receiver_expression in
      let arg_types = args |> List.map (fun (_, {expr_type}) -> expr_type) in
      let constraint_map = build_constraints constraint_map receiver_expression
                           |> append_constraint type_key (FunctionApplication(meth, arg_types, receiver_t)) in
      List.fold_left build_constraints constraint_map args
    | ExprLambda (_, expression) ->
      build_constraints constraint_map expression
      |> append_constraint type_key (Literal(TLambda([TAny], TAny)))
    | _ -> constraint_map
  in match (level, expr_type) with
  | 0, TPoly (type_key) -> build_constraint type_key expr
  | _ -> constraint_map

let print_constraint k v =
  match v with
  | Literal (t) ->
    Printf.printf "\027[31m[CONSTRAINT: Literal (%s = %s)]\027[m\n" k (type_to_str t)
  | FunctionApplication (member, args, receiver_t) ->
    Printf.printf "\027[31m[CONSTRAINT: FunctionApplication (%s -> %s =Fn %s[.%s])]\027[m\n"
      (if List.length args > 0 then 
         (String.concat " -> " (List.map (fun arg_t -> type_to_str arg_t) args))
       else "()")
      k (type_to_str receiver_t) member
  | Equality (a, b) ->
    Printf.printf "\027[31m[CONSTRAINT: Equality (%s = %s)]\027[m\n" (type_to_str a) (type_to_str b)
  | _ ->
    Printf.printf "\027[31m[CONSTRAINT: %s => Unknown]\027[m\n" k

(* AST -> TypedAST *)

let apply_constraints ast constraint_map =
  let _ = constraint_map |> ConstraintMap.iter (fun k vs ->
      vs |> List.iter (fun v -> print_constraint k v)
    ) in ast

let rec to_typed_ast core_expr =
  let constraint_map = ConstraintMap.empty in
  let annotations = annotate core_expr in
  let constraints = build_constraints constraint_map annotations in
  apply_constraints annotations constraints

(* Printer Utility *)

module Printer = struct
  open Core

  let rec print_typed_expr ~indent outc = function
    | ExprCall (receiver, meth, args) ->
      printf "send %a `%s" (print_expression ~indent:(indent+1)) receiver meth;
      (args |> List.iteri ~f:(fun i expr -> print_expression ~indent:(indent+2) outc expr))
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
    let { expr_loc; expr_type; level } = metadata in
    (* printf "%a\n" Location.print_loc expr_loc; *)
    printf "%*s(%s : %a)" indent " " (type_to_str expr_type) (print_typed_expr ~indent:indent) expr;
    if (indent = 1) then printf "\n"
end
