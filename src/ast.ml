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
  | Value of value * t
  | Var of id
  | Assign of string * expr
  | ConstAssign of string * expr
  | Body of expr * expr

and id = string * value * t

let rec expr_return_value = function
  | Var ((_, value, _)) -> value
  | Value (value, _) -> value
  | Call _ -> Any
  | Func (_, _, expr)
  | Body (_, expr)
  | Assign (_, expr)
  | ConstAssign (_, expr) -> expr_return_value expr

let rec expr_return_t = function
  | Var ((_, _, t)) -> t
  | Value (_, t) -> t
  | Call _ -> TAny
  | Body (_, b) -> expr_return_t b
  | Func (_, _, expr) | Assign (_, expr) | ConstAssign (_, expr) -> expr_return_t expr

let id_type (_id, _value, t) = t
let id_value (_id, value, _t) = value
let arg_types args = List.map id_type args
let context_type body = match List.rev body with
| last :: _ -> id_type last
| _ -> TNil
