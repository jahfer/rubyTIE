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
  | Lambda of id list * expr
  | Nil
  | None
  | Any

and expr =
  | Call of expr option * string * expr list (* receiver, method, args *)
  | Func of string * id list * expr (* name, args, body *)
  | Value of id
  | Orphan of value * t
  | Body of expr * expr

and id = string * value * t

let rec expr_return_value = function
  | Value ((_, value, _)) -> value
  | Orphan (value, _) -> value
  | Func (_, _, expr) -> expr_return_value expr
  | Call _ -> Any
  | Body (_, b) -> expr_return_value b

let rec expr_return_t = function
  | Value ((_, _, t)) -> t
  | Orphan (_, t) -> t
  | Func (_, _, expr) -> expr_return_t expr
  | Call _ -> TAny
  | Body (_, b) -> expr_return_t b

let id_type (_id, _value, t) = t
let id_value (_id, value, _t) = value
let arg_types args = List.map id_type args
let context_type body = match List.rev body with
| last :: _ -> id_type last
| _ -> TNil
