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

and id = string * value

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

module AstPrinter = struct
  open Core

  let rec print_cexpr _outc (expr, _) =
    (* printf "%a\n" Location.print_loc expr_loc; *)
    printf "%a" print_ast expr

  and print_ast _outc = function
    | ExprCall (receiver, meth, _args) ->
      printf "(send %a `%s)" print_cexpr receiver meth
    | ExprFunc (name, args, body) ->
      printf "(def `%s %a %a)" name print_args args print_cexpr body
    | ExprLambda (args, body) ->
      printf "(lambda %a %a)" print_args args print_cexpr body
    | ExprVar ((name, _value))  ->
      printf "(lvar `%s)" name
    | ExprConst ((name, _value), base) ->
      printf "(const %a `%s)" print_cexpr base name
    | ExprIVar ((name, _value)) ->
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
    | Array l      -> printf "[%a]" print_list l
    | String s     -> printf "\"%s\"" s
    | Symbol s     -> printf ":%s" s
    | Int i        -> printf "%d" i
    | Float x      -> printf "%f" x
    | Bool true    -> Out_channel.output_string outc "true"
    | Bool false   -> Out_channel.output_string outc "false"
    | Nil          -> Out_channel.output_string outc "nil"
    | Any          -> printf "?"

  and print_args outc arr =
    if List.length(arr) > 0 then begin
      Out_channel.output_string outc "(args";
      List.iteri ~f:(fun _i (id, _value) ->
          Out_channel.output_string outc " ";
          printf "(arg `%s)" id) arr;
      Out_channel.output_string outc ")"
    end else printf "()"

  and print_hash outc obj =
    Out_channel.output_string outc "{ ";
    List.iteri ~f:(fun i (key, value) ->
        if (i <> 0) then printf ", ";
        printf "%a: %a" print_value key print_value value) obj;
    Out_channel.output_string outc " }"

  and print_list outc arr =
    List.iteri ~f:(fun i v ->
        if i > 0 then
          Out_channel.output_string outc " ";
        print_value outc v) arr
end
