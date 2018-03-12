module Untyped_ast = struct
  open Ast
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

module Typed_ast = struct
  open Ast
  open Typed_ast
  open Core

  let rec type_to_str = function
    | THash    -> "hash"
    | TArray t -> sprintf "array<%s>" (type_to_str t)
    | TString  -> "string"
    | TSymbol  -> "symbol"
    | TInt     -> "int"
    | TFloat   -> "float"
    | TConst t -> sprintf "const<%s>" (type_to_str t)
    | TBool    -> "bool"
    | TNil     -> "nil"
    | TAny     -> "any"
    | TLambda (args, ret)  -> sprintf "lambda<%s>" (type_to_str ret)
    | TPoly t  -> t

  let rec print_cexpr outc ({ expr_loc; expr_desc; expr_type }) =
    (* printf "%a\n" Location.print_loc expr_loc; *)
    let core_expr = { expr_loc; expr_desc } in
    printf "%10s : %a" (type_to_str expr_type) Untyped_ast.print_cexpr core_expr
end

(* module TypedAst = struct
  module Type_variable = struct
    let current_var = ref (Char.code 'a')

    let gen_fresh_t () =
      let tv = !current_var in
      incr current_var; Ast.TPoly(Core.sprintf "'%c" (Char.chr tv))

    let reset () = current_var := (Char.code 'a')
  end

  and print_args_t outc arr =
    List.iteri ~f:(fun i t ->
        if i > 0 then
          Out_channel.output_string outc ", ";
        output_sig outc t) arr
end *)
