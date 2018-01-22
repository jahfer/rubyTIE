type t = [
  | `THash
  | `TBool
  | `TFloat
  | `TInt
  | `TArray
  | `TNil
  | `TString
  | `TConst
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
  | `Id of id * value
  | `Const of id * value
  | `Func of (id * value list)
  | `None
]

open Core

let rec output_value outc v =
  match v with
  | `Hash obj     -> print_hash outc obj
  | `Array l      -> printf "[%a]" print_list l
  | `Id ((i, _t), v)    -> printf "%s = %a" i output_value v
  | `Const ((i, _t), v) -> printf "%s = %a" i output_value v
  | `String s     -> printf "\"%s\"" s
  | `Int i        -> printf "%d" i
  | `Float x      -> printf "%f" x
  | `Bool true    -> Out_channel.output_string outc "true"
  | `Bool false   -> Out_channel.output_string outc "false"
  | `Nil          -> Out_channel.output_string outc "nil"
  | `None         -> printf "?"
  | `Func ((id, t), a) -> let args = `Array(a) in
    printf "fun %s(%a) -> %a" id output_value args output_value t

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

let print_type outc = function
  | `TAny -> Out_channel.output_string outc "Any"
  | _ -> Out_channel.output_string outc "?"

let counter = ref 96
let rec output_sig outc v =
  match v with
  | `Hash obj     -> printf "hash" (* print_hash outc obj *)
  | `Array l      -> printf "array<%a>" print_type `TAny
  | `Id ((i, t), _v) -> printf "val %s : %a" i print_type t
  | `Const ((i, t), _v) -> printf "const %s : %a" i  print_type t
  | `String s     -> printf "string"
  | `Int i        -> printf "int"
  | `Float x      -> printf "float"
  | `Bool true    -> Out_channel.output_string outc "true"
  | `Bool false   -> Out_channel.output_string outc "false"
  | `Nil          -> Out_channel.output_string outc "nil"
  | `None         -> counter := !counter + 1; printf "'%c" (char_of_int !counter)
  | `Func ((id, t), a) -> printf "fun %s : (%a) -> %a" id print_args a print_type t

and print_args outc arr =
  List.iteri ~f:(fun i v ->
      if i > 0 then
        Out_channel.output_string outc ", ";
      output_sig outc v) arr
