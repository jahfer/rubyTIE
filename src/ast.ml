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
  | Any

and expr =
  | ExprCall of expr * string * expr list (* receiver, method, args *)
  | ExprFunc of string * id list * expr (* name, args, body *)
  | ExprValue of value
  | ExprVar of id
  | ExprConst of id * expr
  | ExprIVar of id
  | ExprAssign of string * expr
  | ExprIVarAssign of string * expr
  | ExprConstAssign of string * expr
  | ExprBody of expr * expr

and id = string * value

let rec expr_return_value = function
  | ExprVar ((_, value)) | ExprIVar ((_, value)) | ExprConst ((_, value), _) -> value
  | ExprValue (value) -> value
  | ExprCall _ -> Any
  | ExprFunc (_, _, expr)
  | ExprBody (_, expr)
  | ExprAssign (_, expr)
  | ExprIVarAssign (_, expr)
  | ExprConstAssign (_, expr) -> expr_return_value expr
