open Ast

module UntypedAst = struct
  open Core

  let rec print_ast outc = function
    | ExprCall (receiver, meth, args) -> printf "(send %a :%s)" print_ast receiver meth
    | ExprFunc (name, args, body) -> printf "(def :%s %a\n  %a)" name print_args args print_ast body
    | ExprVar (name, value)  -> printf "(lvar :%s)" name
    | ExprConst ((name, value), base) -> printf "(const %a :%s)" print_ast base name
    | ExprIVar (name, value) -> printf "(ivar :%s)" name
    | ExprAssign (name, expr) -> printf "(lvasgn :%s %a)" name print_ast expr
    | ExprIVarAssign (name, expr) -> printf "(ivasgn %s %a)" name print_ast expr
    | ExprConstAssign (name, expr) -> printf "(casgn %s %a)" name print_ast expr
    | ExprValue (value) -> printf "%a" print_value value
    | ExprBody (expr1, expr2) -> printf "%a %a" print_ast expr1 print_ast expr2

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
    | Lambda (args, body) -> printf "(block (lambda) %a\n  %a)" print_args args print_ast body
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


(* module TypedAst = struct
  module Type_variable = struct
    let current_var = ref (Char.code 'a')

    let gen_fresh_t () =
      let tv = !current_var in
      incr current_var; Ast.TPoly(Core.sprintf "'%c" (Char.chr tv))

    let reset () = current_var := (Char.code 'a')
  end

  let rec output_sig outc = function
    | THash    -> printf "hash"
    | TArray _ -> printf "array"
    | TString  -> printf "string"
    | TSymbol  -> printf "symbol"
    | TInt     -> printf "int"
    | TFloat   -> printf "float"
    | TConst _ -> printf "const"
    | TBool    -> Out_channel.output_string outc "bool"
    | TNil     -> Out_channel.output_string outc "nil"
    | TAny     -> printf "any"
    | TLambda (args, ret)  -> printf "lambda"
    | TCall t -> printf "%a" output_sig t
    | TPoly t  -> printf "%s" t

  and print_args_t outc arr =
    List.iteri ~f:(fun i t ->
        if i > 0 then
          Out_channel.output_string outc ", ";
        output_sig outc t) arr
end *)