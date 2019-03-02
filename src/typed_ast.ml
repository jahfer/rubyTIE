open Ast
open Types

(* Define data structures and types for AST *)

module TypeTree = Disjoint_set

(* Printer Utility *)

module ExpressionPrinter = struct
  open Types
  open Ruby_tie__Printer
  open Printf

  let rec print_typed_expr ~indent outc = function
    | ExprCall (receiver, meth, args) ->
      printf "send %a `%s" (print_expression ~indent:(indent+1)) receiver meth;
      (args |> List.iteri (fun _i expr -> print_expression ~indent:(indent+2) outc expr))
    | ExprFunc (name, args, body) ->
      printf "def `%s %a %a" name Ast.AstPrinter.print_args args (print_expression ~indent:(indent+1)) body
    | ExprLambda (args, body) ->
      printf "lambda %a %a" Ast.AstPrinter.print_args args (print_expression ~indent:(indent+1)) body
    | ExprVar ((name, _value))  ->
      printf "lvar `%s" name
    | ExprConst ((name, _value), base) ->
      printf "const %a `%s" (print_expression ~indent:(indent+1)) base name
    | ExprIVar ((name, _value)) ->
      printf "ivar `%s" name
    | ExprAssign (name, expr) ->
      printf "lvasgn `%s %a" name (print_expression ~indent:(indent+1)) expr
    | ExprIVarAssign (name, expr) ->
      printf "ivasgn %s %a" name (print_expression ~indent:(indent+1)) expr
    | ExprConstAssign (name, expr) ->
      printf "casgn %s %a" name (print_expression ~indent:(indent+1)) expr
    | ExprValue (value) ->
      printf "%a" Ast.AstPrinter.print_value value
    | ExprBlock (expr1, expr2) ->
      printf "%a %a" (print_expression ~indent:(indent+1)) expr1 (print_expression ~indent:(indent+1)) expr2

  and print_expression ~indent _outc (expr, metadata) =
    if (indent <> 1) then printf "\n";
    let { type_reference; _ } = metadata in
    (* printf "# %a\n" Location.print_loc expr_loc; *)
    printf "%*s(%s : %a)" indent " "
      (type_to_str type_reference.elem)
      (print_typed_expr ~indent:indent)
      expr;
    if (indent = 1) then printf "\n"

  let print_constraint k v =
    let format_constraint s = Printf.sprintf "\027[31m%s\027[m" s in
    let prefix = format_constraint "CONSTRAINT:" in
    let open Constraint_engine in
    match v with
    | FunctionApplication (member, args, receiver_t) ->
      printf "%s %-20s (%s) -> %s =Fn { %s.%s }\n"
        prefix
        "FunctionApplication"
        (if List.length args > 0 then
           (String.concat ", " (List.map (fun arg ->
                type_to_str (TypeTree.find arg).elem) args))
         else "")
        k (type_to_str (TypeTree.find receiver_t).elem) member
    | Binding (name, t) ->
      printf "%s %-20s %s = %s\n" prefix "Binding" name (type_to_str (TypeTree.find t).elem)
    | Literal (a, t) ->
      printf "%s %-20s %s = %s\n" prefix "Literal" (type_to_str (TypeTree.find a).elem) (type_to_str t)
    | Equality (a, b) ->
      printf "%s %-20s %s = %s\n" prefix "Equality" (type_to_str (TypeTree.find a).elem) (type_to_str (TypeTree.find b).elem)
    | SubType (a, b) ->
      printf "%s %-20s %s < %s\n" prefix "SubType" (type_to_str (TypeTree.find a).elem) (type_to_str (TypeTree.find b).elem)
    | _ ->
      printf "%s %s => Unknown\n" prefix k

  let print_constraint_map constraint_map =
    constraint_map |> Constraint_engine.ConstraintMap.iter (fun k vs ->
        printf "Type variable %s\n" k;
        printf "-----------------\n";
        vs |> List.iter (fun v -> print_constraint k v);
        printf "\n"
      )
end

(* Annotations *)

let annotate expression =
  let annotate_expression expr location_meta =
    let t = Types.gen_fresh_t () in
    let t_node = TypeTree.make ~root:false (Some location_meta) t in
    (expr, { expr_loc = location_meta; type_reference = t_node; level = 0 })
  in let (expr, location_meta) = expression in
  replace_metadata annotate_expression expr location_meta

(* AST -> TypedAST *)
let apply_constraints ast _constraint_map =
  (* TODO: Write actual constraint solver *)
  let annotate_expression expr ({ type_reference; _ } as meta) =
    (expr, { meta with type_reference = TypeTree.find type_reference })
  in let (expr, meta) = ast in
  replace_metadata annotate_expression expr meta
