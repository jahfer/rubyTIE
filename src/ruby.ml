type t =
  | THash
  | TBool
  | TFloat
  | TInt
  | TArray of t
  | TNil
  | TString
  | TConst of t
  | TFunc of t
  | TAny

and value =
  | Hash of (value * value) list
  | Bool of bool
  | Float of float
  | Int of int
  | Array of value list
  | Nil
  | String of string
  | Func of value list
  | None

and id = string * value * t

let gen_id_name = let counter = ref 96 in
  counter := !counter + 1; String.make 1 (char_of_int !counter)

open Core

let id_type (_, _, t) = t
let id_value (_, v, _) = v

let rec rb_typeof = function
  | Hash _ -> THash
  | Bool _ -> TBool
  | Float _ -> TFloat
  | Int _ -> TInt
  | Array _ -> TArray (TAny)
  | Nil -> TNil
  | String _ -> TString
  | Func _ -> TFunc (TAny)
  | None -> TAny

let counter = ref 96
let rec output_sig outc = function
  | THash    -> printf "hash" (* print_hash outc obj *)
  | TArray l -> printf "array<%a>" output_sig TAny
  | TString  -> printf "string"
  | TInt     -> printf "int"
  | TFloat   -> printf "float"
  | TConst t -> printf "const<%a>" output_sig t
  | TBool    -> Out_channel.output_string outc "bool"
  | TNil     -> Out_channel.output_string outc "nil"
  | TAny     -> counter := !counter + 1; printf "'%c" (char_of_int !counter)
  | TFunc a  -> printf "fun : ?"

let rec output_value outc value =
  match value with
  | Hash obj     -> print_hash outc obj
  | Array l      -> printf "[%a]" print_list l
  | String s     -> printf "\"%s\"" s
  | Int i        -> printf "%d" i
  | Float x      -> printf "%f" x
  | Bool true    -> Out_channel.output_string outc "true"
  | Bool false   -> Out_channel.output_string outc "false"
  | Nil          -> Out_channel.output_string outc "nil"
  | None         -> printf "?"
  | Func a -> let args = Array(a) in
    printf "fun (%a) -> %a" output_value args output_sig TAny

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

and print_args outc arr =
  List.iteri ~f:(fun i v ->
      if i > 0 then
        Out_channel.output_string outc ", ";
      output_sig outc v) arr
