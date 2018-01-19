type value = [
  | `Hash of (value * value) list
  | `Bool of bool
  | `Float of float
  | `Int of int
  | `List of value list
  | `Nil
  | `String of string
  | `Id of (string * value)
  | `Const of (string * value)
  | `Func of (value * value list)
  | `Any
]

open Core

let rec output_value outc v =
  match v with
  | `Hash obj     -> printf "{?}" (* print_hash outc obj *)
  | `List l       -> print_list outc l
  | `Id (i, t)    -> printf "%s = %a" i output_value t
  | `Const (i, t) -> printf "%s = %a" i output_value t
  | `String s     -> printf "\"%s\"" s
  | `Int i        -> printf "%d" i
  | `Float x      -> printf "%f" x
  | `Bool true    -> Out_channel.output_string outc "true"
  | `Bool false   -> Out_channel.output_string outc "false"
  | `Nil          -> Out_channel.output_string outc "nil"
  | `Any          -> printf "?"
  | `Func (id, args) -> printf "func %a" output_value id; print_list outc args

(* and print_hash outc obj =
  Out_channel.output_string outc "{ ";
  let sep = ref "" in
  List.iter ~f:(fun (key, value) ->
      printf "%s\"%a\": %a" !sep key (output_value value);
      sep := ",\n  ") obj;
  Out_channel.output_string outc " }" *)

and print_list outc arr =
  Out_channel.output_string outc "[";
  List.iteri ~f:(fun i v ->
      if i > 0 then
        Out_channel.output_string outc ", ";
      output_value outc v) arr;
Out_channel.output_string outc "]"

let counter = ref 96
let rec output_sig outc v =
  match v with
  | `Hash obj     -> printf "hash" (* print_hash outc obj *)
  | `List l       -> print_list outc l
  | `Id (i, t)    -> printf "val %s : %a" i output_sig t
  | `Const (i, t) -> printf "const %s : %a" i output_sig t
  | `String s     -> printf "string"
  | `Int i        -> printf "int"
  | `Float x      -> printf "float"
  | `Bool true    -> Out_channel.output_string outc "true"
  | `Bool false   -> Out_channel.output_string outc "false"
  | `Nil          -> Out_channel.output_string outc "nil"
  | `Any          -> counter := !counter + 1; printf "'%c" (char_of_int !counter)
  | `Func (id, args) -> printf "func %a" output_sig id; print_list outc args
