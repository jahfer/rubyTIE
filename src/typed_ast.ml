open Ast
open Types

(* Define data structures and types for AST *)

module TypeTree = Disjoint_set

(* Annotations *)

module Annotations = struct
  let annotate expression =
    let rec annotate_expression expr location_meta =
      let t = Types.gen_fresh_t () in
      let t_node = TypeTree.make t ~root:false in
      (expr, { expr_loc = location_meta; type_reference = t_node; level = 0 })
    in let (expr, location_meta) = expression in
    replace_metadata annotate_expression expr location_meta
end

(* AST -> TypedAST *)
let apply_constraints ast constraint_map =
  let annotate_expression expr ({ type_reference } as meta) =
    (expr, { meta with type_reference = TypeTree.find type_reference })
  in let (expr, meta) = ast in
  replace_metadata annotate_expression expr meta

(* Printer Utility *)

module ExpressionPrinter = struct
  open Types
  open Typed_ruby__Printer
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
    let open Constraint_engine in
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
    constraint_map |> Constraint_engine.ConstraintMap.iter (fun k vs ->
        vs |> List.iter (fun v -> print_constraint k v)
      )

  let debug_union (a : Types.t TypeTree.t) (b : Types.t TypeTree.t) =
    Printf.printf "-- Unifying %s and %s\n" (type_to_str a.elem) (type_to_str b.elem)
end

let rec to_typed_ast core_expr =
  let open Constraint_engine in
  let constraint_map = ConstraintMap.empty in
  (* let reference_map = ReferenceMap.empty in *)
  let annotations = Annotations.annotate core_expr in
  let constraints = build_constraints constraint_map annotations in
  let _ = ExpressionPrinter.print_constraint_map constraints in
  apply_constraints annotations constraints
