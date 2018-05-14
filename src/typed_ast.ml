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

type constraint_t =
  | Literal of t
  | Member of string * t (* member name, return value *)
  | Equality of t * t
  | Disjuction of constraint_t list
  | Overload of t
  | Class of t

type metadata = {
  expr_loc : Location.t;
  expr_type : t;
  level : int;
}

(* Printer Utility *)

module Printer = struct
  open Core

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
      sprintf "const<%s>" (type_to_str t)
    | TArray t ->
      sprintf "array<%s>" (type_to_str t)
    | TLambda (args, ret) -> 
      sprintf "lambda<args -> %s>" (type_to_str ret)
    | TFunc (args, ret) ->
      sprintf "func<args -> %s>" (type_to_str ret)
    | TPoly t ->
      sprintf "%s" t

  let rec print_typed_expr outc = function
    | ExprCall (receiver, meth, args) ->
      printf "send %a `%s" print_expression receiver meth
    | ExprFunc (name, args, body) ->
      printf "def `%s %a %a" name Ast.Printer.print_args args print_expression body
    | ExprVar ((name, value))  ->
      printf "lvar `%s" name
    | ExprConst ((name, value), base) ->
      printf "const %a `%s" print_expression base name
    | ExprIVar ((name, value)) ->
      printf "ivar `%s" name
    | ExprAssign (name, expr) ->
      printf "lvasgn `%s %a" name print_expression expr
    | ExprIVarAssign (name, expr) ->
      printf "ivasgn %s %a" name print_expression expr
    | ExprConstAssign (name, expr) ->
      printf "casgn %s %a" name print_expression expr
    | ExprValue (value) ->
      printf "%a" Ast.Printer.print_value value
    | ExprBody (expr1, expr2) ->
      printf "%a %a" print_expression expr1 print_expression expr2

  and print_expression outc (expr, metadata) =
    let { expr_loc; expr_type; level } = metadata in
    (* printf "%a\n" Location.print_loc expr_loc; *)
    printf "(%s : %a)" (type_to_str expr_type) print_typed_expr expr

  let print_constraint k v =
    match v with
    | Literal (t) ->
      Printf.printf "\027[31m[CONSTRAINT: Literal (%s = %s)]\027[m\n" k (type_to_str t)
    | Member (member, return_t) ->
      Printf.printf "\027[31m[CONSTRAINT: Member (%s.%s -> %s)]\027[m\n" k member (type_to_str return_t)
    | Equality (a, b) ->
      Printf.printf "\027[31m[CONSTRAINT: Equality (%s = %s)]\027[m\n" (type_to_str a) (type_to_str b)
    | _ ->
      Printf.printf "\027[31m[CONSTRAINT: %s => Unknown]\027[m\n" k
end

(* Build map and generator for unique type names *)

let current_var = ref 1
let gen_fresh_t () =
  let tv = !current_var in incr current_var;
  TPoly(Core.sprintf "t/1%03i" tv)

let rec typeof_value = function
  | Hash _   -> THash
  | Bool _   -> TBool
  | Float _  -> TFloat
  | Int _    -> TInt
  | Array _  -> TArray (gen_fresh_t ())
  | String _ -> TString
  | Symbol _ -> TSymbol
  (* TODO: Lambda *)
  | Lambda (args) -> TLambda ([TAny], TAny)
  | Nil      -> TNil
  | Any      -> gen_fresh_t ()

and typeof_expr expr = expr |> expr_return_value |> typeof_value

(* Constraint Generation *)

module ConstraintMap = Map.Make (String)

let append_constraint k c map =
    let lst = match ConstraintMap.find_opt k map with
    | Some(lst) -> lst
    | None -> []
    in map |> ConstraintMap.add k (c :: lst)

let build_constraints constraint_map (expr, { expr_type; level }) =
  let build_constraint type_key = function
  | ExprValue(v) -> 
    constraint_map
    |> append_constraint type_key (Literal (typeof_value v))
  | ExprAssign (v, iexpr)
  | ExprIVarAssign (v, iexpr)
  | ExprConstAssign (v, iexpr) ->
    let typ = typeof_expr expr in begin
    match typ with
    | TPoly t ->
      append_constraint t (Equality (typ, expr_type)) constraint_map
    | TLambda (_, (TPoly(t) as poly_t)) ->
      constraint_map
      |> append_constraint t (Equality (poly_t, expr_type))
      |> append_constraint type_key (Literal typ)
    | _ -> append_constraint type_key (Literal typ) constraint_map
    end
  | ExprCall ((receiver_expr, _), meth, _args) ->
    let return_typ = typeof_expr receiver_expr in
    constraint_map
    |> append_constraint type_key (Member(meth, return_typ))
  | _ -> constraint_map
  in match (level, expr_type) with
  | 0, TPoly (type_key) -> build_constraint type_key expr
  | _ -> constraint_map

(* Annotations *)

module AnnotationMap = Map.Make (String)
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
    gen_typ

and annotate expression =
  let rec annotate_expression expr location_meta =
    let t = gen_fresh_t ()
    in (expr, { expr_loc = location_meta; expr_type = t; level = 0 })
  in let (expr, location_meta) = expression in
  replace_metadata annotate_expression expr location_meta

(* AST -> TypedAST *)

let apply_constraints ast constraint_map =
  let _ = constraint_map |> ConstraintMap.iter (fun k vs ->
    vs |> List.iter (fun v -> Printer.print_constraint k v)
  ) in ast

let rec to_typed_ast core_expr =
  let constraint_map = ConstraintMap.empty in
  let annotations = annotate core_expr in
  let constraints = build_constraints constraint_map annotations in
  apply_constraints annotations constraints


