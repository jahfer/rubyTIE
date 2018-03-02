open Ast

(*
  need locals map!

  b = 3 : int
  a = b : any <-- WRONG
*)

module Type_variable = struct
  let current_var = ref (Char.code 'a')

  let gen_new_t () =
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
  | Array _ -> TArray (Type_variable.gen_new_t ())
  | Nil -> TNil
  | String _ -> TString
  | Symbol _ -> TSymbol
  | Lambda (args, body) -> TLambda (arg_types args, Ast.expr_return_t (body))
  | None -> TAny
  | Any -> TAny

module Printer = struct
  let rec output_sig outc = function
    | THash    -> printf "hash"
    | TArray t -> printf "array<%a>" output_sig t
    | TString  -> printf "string"
    | TSymbol  -> printf "symbol"
    | TInt     -> printf "int"
    | TFloat   -> printf "float"
    | TConst t -> printf "const<%a>" output_sig t
    | TBool    -> Out_channel.output_string outc "bool"
    | TNil     -> Out_channel.output_string outc "nil"
    | TAny     -> printf "any"
    | TLambda (args, ret)  -> printf "lambda (%a) -> %a" print_args_t args output_sig ret
    | TCall t -> printf "%a" output_sig t
    | TPoly t  -> printf "%s" t

  and print_args_t outc arr =
    List.iteri ~f:(fun i t ->
        if i > 0 then
          Out_channel.output_string outc ", ";
        output_sig outc t) arr

  let rec print_value outc = function
    | Hash obj     -> print_hash outc obj
    | Array l      -> printf "[%a]" print_list l
    | String s     -> printf "\"%s\"" s
    | Symbol s     -> printf ":%s" s
    | Int i        -> printf "%d" i
    | Float x      -> printf "%f" x
    | Bool true    -> Out_channel.output_string outc "true"
    | Bool false   -> Out_channel.output_string outc "false"
    | Nil          -> Out_channel.output_string outc "nil"
    | None         -> printf "?"
    | Lambda _     -> printf "-> { ... }"
    | Any          -> printf "?"

  and print_hash outc obj =
    Out_channel.output_string outc "{ ";
    let sep = ref "" in
    List.iter ~f:(fun (key, value) ->
        printf "%s%a: %a" !sep print_value key print_value value;
        sep := ",\n  ") obj;
    Out_channel.output_string outc " }"

  and print_list outc arr =
    List.iteri ~f:(fun i v ->
        if i > 0 then
          Out_channel.output_string outc ", ";
        print_value outc v) arr

  and print_signature outc (id, value, typ) = match typ with
    | _ -> printf "%s : %a = %a" id output_sig typ print_value value

  and print_args outc arr =
    List.iteri ~f:(fun i t ->
        if i > 0 then
          Out_channel.output_string outc ", ";
        print_signature outc t) arr

  let rec print_expr outc = function
    | Call (receiver, meth, args) -> begin
      match receiver with
      | Some(expr) -> printf "(%a).%s (...)" print_expr expr meth
      | None -> printf "self.%s (...)" meth
    end
    | Func (name, args, body) -> printf "%s : fun (%a) -> %a" name print_args args print_expr body
    | Value id -> print_signature outc id
    | Orphan (value, t) -> printf "%a" print_signature ("(orphan)", value, t)
    | Body (expr1, expr2) -> print_expr outc expr2
end
