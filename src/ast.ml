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

let id_type (_id, _value, t) = t
let arg_types args = List.map id_type args
