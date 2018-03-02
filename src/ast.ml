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
  | TLambda of t list * t
  | TCall of t

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
  | Lambda of id list * id list
  | Call of id option * string * id list (* receiver, method, args *)

and id = string * value * t

let id_type (_id, _value, t) = t
let id_value (_id, value, _t) = value
let arg_types args = List.map id_type args
let context_type body = match List.rev body with
| last :: _ -> id_type last
| _ -> TNil
