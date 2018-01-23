type t = [
  | `THash
  | `TBool
  | `TFloat
  | `TInt
  | `TArray of t
  | `TNil
  | `TString
  | `TConst of t
  | `TFunc
  | `TAny
]

type id = string * t
and value = [
  | `Hash of (value * value) list
  | `Bool of bool
  | `Float of float
  | `Int of int
  | `Array of value list
  | `Nil
  | `String of string
  | `Id of string * value
  | `Const of string * value
  | `Func of string * value list
  | `None
]

open Core

let rec rb_typeof = function
  | `Hash v -> `THash
  | `Bool b -> `TBool
  | `Float f -> `TFloat
  | `Int i -> `TInt
  | `Array a -> `TArray (`TAny)
  | `Nil -> `TNil
  | `String s -> `TString
  | `Id (id, v) -> `TId
  | `Const (id, v) -> `TConst (rb_typeof v)
  | `Func (id, args) -> `TFunc
  | `None -> `TAny

let print_type outc value =
  let rec str_type = function
  | `THash -> "Hash"
  | `TBool -> "Boolean"
  | `TFloat -> "Float"
  | `TInt -> "Integer"
  | `TArray t -> sprintf "Array<%s>" (str_type t)
  | `TNil -> "Nil"
  | `TString -> "String"
  | `TConst t -> sprintf "Constant<%s>" (str_type t)
  | `TFunc -> "Function"
  | `TAny -> "Any"
  | _ -> "?"
  in Out_channel.output_string outc (str_type value)

let rec output_value outc v =
  match v with
  | `Hash obj     -> print_hash outc obj
  | `Array l      -> printf "[%a]" print_list l
  | `Id (id, v)    -> printf "%s = %a" id output_value v
  | `Const (id, v) -> printf "%s = %a" id output_value v
  | `String s     -> printf "\"%s\"" s
  | `Int i        -> printf "%d" i
  | `Float x      -> printf "%f" x
  | `Bool true    -> Out_channel.output_string outc "true"
  | `Bool false   -> Out_channel.output_string outc "false"
  | `Nil          -> Out_channel.output_string outc "nil"
  | `None         -> printf "?"
  | `Func (id, a) -> let args = `Array(a) in
    printf "fun %s(%a) -> %a" id output_value args print_type `TAny

and print_hash outc obj =
  Out_channel.output_string outc "{ ";
  let sep = ref "" in
  List.iter ~f:(fun (key, value) ->
      printf "%s%a: %a" !sep output_value key output_value value;
      sep := ",\n  ") obj;
  Out_channel.output_string outc " }"

and print_list outc arr =
  List.iteri ~f:(fun i v ->
      if i > 0 then
        Out_channel.output_string outc ", ";
      output_value outc v) arr

let counter = ref 96
let rec output_sig outc v =
  match v with
  | `Hash obj     -> printf "hash" (* print_hash outc obj *)
  | `Array l      -> printf "array<%a>" print_type `TAny
  | `Id (id, v)   -> printf "val %s : %a" id print_type (rb_typeof v)
  | `Const (id, v) as c -> printf "const %s : %a" id print_type (rb_typeof c)
  | `String s     -> printf "string"
  | `Int i        -> printf "int"
  | `Float x      -> printf "float"
  | `Bool true    -> Out_channel.output_string outc "true"
  | `Bool false   -> Out_channel.output_string outc "false"
  | `Nil          -> Out_channel.output_string outc "nil"
  | `None         -> counter := !counter + 1; printf "'%c" (char_of_int !counter)
  | `Func (id, a) -> printf "fun %s : (%a) -> %a" id print_args a print_type `TAny

and print_args outc arr =
  List.iteri ~f:(fun i v ->
      if i > 0 then
        Out_channel.output_string outc ", ";
      output_sig outc v) arr
