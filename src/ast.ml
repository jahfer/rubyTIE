type value =
  | Hash of (value * value) list
  | Bool of bool
  | Float of float
  | Int of int
  | Array of value list
  | String of string
  | Symbol of string
  | Lambda of id list * core_expression
  | Nil
  | Any

and expr =
  | ExprCall of core_expression * string * core_expression list (* receiver, method, args *)
  | ExprFunc of string * id list * core_expression (* name, args, body *)
  | ExprValue of value
  | ExprVar of id
  | ExprConst of id * core_expression
  | ExprIVar of id
  | ExprAssign of string * core_expression
  | ExprIVarAssign of string * core_expression
  | ExprConstAssign of string * core_expression
  | ExprBody of core_expression * core_expression

and id = string * value

and core_expression = {
  expr_desc : expr;
  expr_loc : Location.t;
}

let rec expr_return_value = function
  | ExprVar ((_, value)) | ExprIVar ((_, value)) | ExprConst ((_, value), _) -> value
  | ExprValue (value) -> value
  | ExprCall _ -> Any
  | ExprFunc (_, _, { expr_desc })
  | ExprBody (_, { expr_desc })
  | ExprAssign (_, { expr_desc })
  | ExprIVarAssign (_, { expr_desc })
  | ExprConstAssign (_, { expr_desc }) -> expr_return_value expr_desc

module Printer = struct
  open Core

  let rec print_cexpr outc { expr_loc; expr_desc } =
    (* printf "%a\n" Location.print_loc expr_loc; *)
    printf "%a" print_ast expr_desc

  and print_ast outc = function
    | ExprCall (receiver, meth, args) -> printf "(send %a :%s)" print_cexpr receiver meth
    | ExprFunc (name, args, body) -> printf "(def :%s %a %a)" name print_args args print_cexpr body
    | ExprVar (name, value)  -> printf "(lvar :%s)" name
    | ExprConst ((name, value), base) -> printf "(const %a :%s)" print_cexpr base name
    | ExprIVar (name, value) -> printf "(ivar :%s)" name
    | ExprAssign (name, expr) -> printf "(lvasgn :%s %a)" name print_cexpr expr
    | ExprIVarAssign (name, expr) -> printf "(ivasgn %s %a)" name print_cexpr expr
    | ExprConstAssign (name, expr) -> printf "(casgn %s %a)" name print_cexpr expr
    | ExprValue (value) -> printf "%a" print_value value
    | ExprBody (expr1, expr2) -> printf "%a %a" print_cexpr expr1 print_cexpr expr2

  and print_value outc = function
    | Hash obj     -> print_hash outc obj
    | Array l      -> printf "(array %a)" print_list l
    | String s     -> printf "(str \"%s\")" s
    | Symbol s     -> printf "(sym :%s)" s
    | Int i        -> printf "(int %d)" i
    | Float x      -> printf "(float %f)" x
    | Bool true    -> Out_channel.output_string outc "(true)"
    | Bool false   -> Out_channel.output_string outc "(false)"
    | Nil          -> Out_channel.output_string outc "(nil)"
    | Lambda (args, { expr_desc = body }) -> printf "(block (lambda) %a %a)" print_args args print_ast body
    | Any          -> printf "?"

  and print_args outc arr =
    Out_channel.output_string outc "(args";
      List.iteri ~f:(fun i (id, value) ->
        Out_channel.output_string outc " ";
        printf "(arg :%s)" id) arr;
    Out_channel.output_string outc ")"

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
