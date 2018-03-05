open Ast

(*
  need locals map!

  b = 3 : int
  a = b : any <-- WRONG
*)

module Type_variable = struct
  let current_var = ref (Char.code 'a')

  let gen_fresh_t () =
    let tv = !current_var in
    incr current_var; Ast.TPoly(Core.sprintf "'%c" (Char.chr tv))

  let reset () = current_var := (Char.code 'a')
end

open Core

let rec typeof = function
  | Hash _ -> THash
  | Bool _ -> TBool
  | Float _ -> TFloat
  | Int _ -> TInt
  | Array _ -> TArray (Type_variable.gen_fresh_t ())
  | Nil -> TNil
  | String _ -> TString
  | Symbol _ -> TSymbol
  | Lambda (args, body) -> TLambda (arg_types args, Ast.expr_return_t (body))
  | None -> TAny
  | Any -> TAny

module Printer = struct
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
    | TLambda (args, ret)  -> printf "block"
    | TCall t -> printf "%a" output_sig t
    | TPoly t  -> printf "%s" t

  and print_args_t outc arr =
    List.iteri ~f:(fun i t ->
        if i > 0 then
          Out_channel.output_string outc ", ";
        output_sig outc t) arr

  let rec print_value outc = function
    | Hash obj     -> print_hash outc obj
    | Array l      -> printf "(array %a)" print_list l
    | String s     -> printf "(str \"%s\")" s
    | Symbol s     -> printf "(sym :%s)" s
    | Int i        -> printf "(int %d)" i
    | Float x      -> printf "(float %f)" x
    | Bool true    -> Out_channel.output_string outc "(true)"
    | Bool false   -> Out_channel.output_string outc "(false)"
    | Nil          -> Out_channel.output_string outc "(nil)"
    | None         -> printf "(?("
    | Lambda (args, _) -> printf "(lambda) %a (BODY)" print_args args
    | Any          -> printf "?"

  and print_hash outc obj =
    Out_channel.output_string outc "(hash ";
    List.iter ~f:(fun (key, value) ->
        printf "(pair %a %a)" print_value key print_value value) obj;
    Out_channel.output_string outc ")"

  and print_list outc arr =
    List.iteri ~f:(fun i v ->
        if i > 0 then
          Out_channel.output_string outc ", ";
        print_value outc v) arr

  and print_signature outc (id, value, typ) = match typ with
    | _ -> printf "%s : %a = %a" id output_sig typ print_value value

  and print_args outc arr =
    Out_channel.output_string outc "(args";
      List.iteri ~f:(fun i (id, value, typ) ->
        Out_channel.output_string outc " ";
        printf "(arg :%s)" id) arr;
    Out_channel.output_string outc ")"

  let rec print_expr_ast outc = function
    | Call (receiver, meth, args) -> begin
      match receiver with
      | Some(expr) -> printf "(send %a :%s)" print_expr_ast expr meth
      | None -> printf "(send (self) :%s)" meth
    end
    | Func (name, args, body) -> printf "(def :%s %a %a)" name print_args args print_expr_ast body
    | Var (name, value, t) -> printf "(lvar :%s)" name
    | Assign (name, expr) -> printf "(lvasgn :%s %a)" name print_expr_ast expr
    | ConstAssign (name, expr) -> printf "(casgn %s %a)" name print_expr_ast expr
    | Value (value, t) -> printf "%a" print_value value
    | Body (expr1, expr2) -> printf "%a %a" print_expr_ast expr1 print_expr_ast expr2
end
