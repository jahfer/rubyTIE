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
  | Lambda of id list * context
  | BinOp of binop * id * id

and id = string * value * t
and statement = id list
and context = statement list
and binop = Add | Sub | Mul | Div

let id_type (_id, _value, t) = t
let arg_types args = List.map id_type args
let rec context_type = function
  | stmt :: stmts -> context_type stmts
  | stmt -> context_type stmt
