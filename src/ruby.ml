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
  | TFunc of t list * t
  | TAny
  | TPoly of string

and value =
  | Hash of (value * value) list
  | Bool of bool
  | Float of float
  | Int of int
  | Array of value list
  | String of string
  | Symbol of string
  | Func of id list
  | Nil
  | None

and id = string * value * t

let type_variable = ref (Char.code 'a')

let gen_polymorphic_type () =
  let tv = !type_variable in
  incr type_variable; TPoly(Core.sprintf "'%c" (Char.chr tv))

let reset_type_variable () = type_variable := (Char.code 'a')

let id_type (_id, _value, t) = t
let arg_types args = List.map id_type args

open Core

let rec rb_typeof = function
  | Hash _ -> THash
  | Bool _ -> TBool
  | Float _ -> TFloat
  | Int _ -> TInt
  | Array _ -> TArray (gen_polymorphic_type ())
  | Nil -> TNil
  | String _ -> TString
  | Symbol _ -> TSymbol
  | Func args -> TFunc (arg_types args, (gen_polymorphic_type ()))
  | None -> TAny

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
  | TFunc (args, ret)  -> printf "fun (%a) -> %a" print_args args output_sig ret
  | TPoly t  -> printf "%s" t

and print_args outc arr =
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
  | Func args    -> printf "fun { ... }"

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
  | _ -> printf "val %s : %a = %a" id output_sig typ print_value value
