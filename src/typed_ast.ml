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
  type_reference : t Disjoint_set.t;
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
    let { type_reference } = metadata in
    type_reference.elem

(* Annotations *)

let annotate expression =
  let rec annotate_expression expr location_meta =
    let t = gen_fresh_t () in
    let wrapped_t = Disjoint_set.make t in
    (expr, { expr_loc = location_meta; type_reference = wrapped_t; level = 0 })
  in let (expr, location_meta) = expression in
  replace_metadata annotate_expression expr location_meta

(* Constraint Generation *)

let type_entry =
  let t_hash = Disjoint_set.make THash in
  function
  | TBool
  | TFloat
  | TInt
  | TArray _
  | TNil
  | TString
  | TSymbol
  | TConst _
  | TAny
  | TPoly _
  | TLambda _
  | TFunc _ 
  | THash -> t_hash

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

let rec build_constraints constraint_map (expr, { type_reference; level }) =
  let expr_t = type_reference.elem in
  let build_constraint type_key = function
    | ExprValue(v) ->
      constraint_map
      |> append_constraint type_key (Literal (typeof_value v))
    | ExprAssign (v, ((_, metadata) as iexpr))
    | ExprIVarAssign (v, ((_, metadata) as iexpr))
    | ExprConstAssign (v, ((_, metadata) as iexpr)) ->
      Disjoint_set.union type_reference metadata.type_reference;
      let constraint_map = build_constraints constraint_map iexpr in
      let typ = typeof_expr expr in begin match typ with
        | TPoly t ->
          append_constraint t (Equality (typ, expr_t)) constraint_map
        | TLambda (_, (TPoly(t) as poly_t)) ->
          constraint_map
          |> append_constraint t (Equality (poly_t, expr_t))
          |> append_constraint type_key (Literal typ)
        | _ -> append_constraint type_key (Literal typ) constraint_map
      end
    | ExprCall (receiver_expression, meth, args) ->
      let (_, {type_reference = receiver}) = receiver_expression in
      let receiver_t = receiver.elem in
      let arg_types = args |> List.map (fun (_, {type_reference}) -> type_reference.elem) in
      let constraint_map = build_constraints constraint_map receiver_expression
                           |> append_constraint type_key (FunctionApplication(meth, arg_types, receiver_t)) in
      List.fold_left build_constraints constraint_map args
    | ExprLambda (_, expression) ->
      let (_, {type_reference = return_type_wrapper}) = expression in
      let return_t = return_type_wrapper.elem in
      build_constraints constraint_map expression
      |> append_constraint type_key (Literal(TLambda([TAny], return_t)))
    | _ -> constraint_map
  in match (level, expr_t) with
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

(* let apply_constraints ast constraint_map =
  let rec annotate_expression expr ({ type_reference } as meta) =
    match type_reference with
    | TPoly (type_key) -> 
      let bound_type = ConstraintMap.mem type_key constraint_map in
      if bound_type then
        let literal_constraint = constraint_map
                                 |> ConstraintMap.find(type_key)
                                 |> List.find_opt (function | Literal(_) -> true | _ -> false) in
        (match literal_constraint with
        | Some(Literal(t)) -> (expr, { meta with type_reference = t; level = 0 })
        | _ -> (expr, meta))
      else (expr, meta)
    | _ -> (expr, meta)
  in let (expr, meta) = ast in
  replace_metadata annotate_expression expr meta *)

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
    let { expr_loc; type_reference; level } = metadata in
    (* printf "%a\n" Location.print_loc expr_loc; *)
    printf "%*s(%s : %a)" indent " "
      (type_to_str (Disjoint_set.find type_reference).elem)
      (* (type_to_str type_reference.elem) *)
      (print_typed_expr ~indent:indent)
      expr;
    if (indent = 1) then printf "\n"
end
