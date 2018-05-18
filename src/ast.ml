type value =
  | Hash of (value * value) list
  | Bool of bool
  | Float of float
  | Int of int
  | Array of value list
  | String of string
  | Symbol of string
  | Nil
  | Any

and 'a expr =
  | ExprCall of 'a expression * string * 'a expression list (* receiver, method, args *)
  | ExprFunc of string * id list * 'a expression (* name, args, body *)
  | ExprLambda of id list * 'a expression (* args, body *)
  | ExprValue of value
  | ExprVar of id
  | ExprConst of id * 'a expression
  | ExprIVar of id
  | ExprAssign of string * 'a expression
  | ExprIVarAssign of string * 'a expression
  | ExprConstAssign of string * 'a expression
  | ExprBlock of 'a expression * 'a expression

and id = string * value
and 'a expression = 'a expr * 'a

let rec replace_metadata fn expr meta =
  let swap_meta = replace_metadata fn in
  let new_expr = match expr with
  | ExprFunc (name, args, (body_expr, body_meta)) ->
    ExprFunc (name, args, swap_meta body_expr body_meta)
  | ExprLambda (args, (body_expr, body_meta)) ->
    ExprLambda (args, swap_meta body_expr body_meta)
  | ExprConst (name, (c_expr, c_meta)) ->
    ExprConst (name, swap_meta c_expr c_meta)
  | ExprAssign (name, (a_expr, a_meta)) ->
    ExprAssign (name, swap_meta a_expr a_meta)
  | ExprIVarAssign (name, (a_expr, a_meta)) ->
    ExprIVarAssign (name, swap_meta a_expr a_meta)
  | ExprConstAssign (name, (a_expr, a_meta)) ->
    ExprConstAssign (name, swap_meta a_expr a_meta)
  | ExprIVar name -> ExprIVar name
  | ExprVar name -> ExprVar name
  | ExprValue v -> ExprValue v
  | ExprCall ((expr_a, meta_a), b, args) ->
    let new_expr = swap_meta expr_a meta_a
    and new_args = List.map (fun (e, m) -> swap_meta e m) args
    in ExprCall (new_expr, b, new_args)
  | ExprBlock ((expr_a, meta_a), (expr_b, meta_b)) ->
    let a = swap_meta expr_a meta_a
    and b = swap_meta expr_b meta_b
    in ExprBlock (a, b)
  in fn new_expr meta

let rec expr_return_value = function
  | ExprCall _ -> Any
  | ExprVar ((_, value))
  | ExprIVar ((_, value))
  | ExprConst ((_, value), _)
  | ExprValue (value) -> value
  | ExprFunc (_, _, (expr, _))
  | ExprLambda (_, (expr, _))
  | ExprBlock (_, (expr, _))
  | ExprAssign (_, (expr, _))
  | ExprIVarAssign (_, (expr, _))
  | ExprConstAssign (_, (expr, _)) -> expr_return_value expr

module Printer = struct
  open Core

  let rec print_cexpr outc (expr, _) =
    (* printf "%a\n" Location.print_loc expr_loc; *)
    printf "%a" print_ast expr

  and print_ast outc = function
    | ExprCall (receiver, meth, args) ->
      printf "(send %a `%s)" print_cexpr receiver meth
    | ExprFunc (name, args, body) ->
      printf "(def `%s %a %a)" name print_args args print_cexpr body
    | ExprLambda (args, body) ->
      printf "(lambda %a %a)" print_args args print_cexpr body
    | ExprVar ((name, value))  ->
      printf "(lvar `%s)" name
    | ExprConst ((name, value), base) ->
      printf "(const %a `%s)" print_cexpr base name
    | ExprIVar ((name, value)) ->
      printf "(ivar `%s)" name
    | ExprAssign (name, expr) ->
      printf "(lvasgn `%s %a)" name print_cexpr expr
    | ExprIVarAssign (name, expr) ->
      printf "(ivasgn %s %a)" name print_cexpr expr
    | ExprConstAssign (name, expr) ->
      printf "(casgn %s %a)" name print_cexpr expr
    | ExprValue (value) ->
      printf "%a" print_value value
    | ExprBlock (expr1, expr2) ->
      printf "%a %a" print_cexpr expr1 print_cexpr expr2

  and print_value outc = function
    | Hash obj     -> print_hash outc obj
    | Array l      -> printf "(array %a)" print_list l
    | String s     -> printf "(str \"%s\")" s
    | Symbol s     -> printf "(sym `%s)" s
    | Int i        -> printf "(int %d)" i
    | Float x      -> printf "(float %f)" x
    | Bool true    -> Out_channel.output_string outc "(true)"
    | Bool false   -> Out_channel.output_string outc "(false)"
    | Nil          -> Out_channel.output_string outc "(nil)"
    | Any          -> printf "?"

  and print_args outc arr =
    if List.length(arr) > 0 then begin
      Out_channel.output_string outc "(args";
        List.iteri ~f:(fun i (id, value) ->
          Out_channel.output_string outc " ";
          printf "(arg `%s)" id) arr;
      Out_channel.output_string outc ")"
    end else printf "()"

  and print_hash outc obj =
    Out_channel.output_string outc "(hash";
    List.iter ~f:(fun (key, value) ->
        printf " (pair %a %a)" print_value key print_value value) obj;
    Out_channel.output_string outc ")"

  and print_list outc arr =
    List.iteri ~f:(fun i v ->
        if i > 0 then
          Out_channel.output_string outc " ";
        print_value outc v) arr
end
