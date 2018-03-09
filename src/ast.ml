type value =
  | Hash of (value * value) list
  | Bool of bool
  | Float of float
  | Int of int
  | Array of value list
  | String of string
  | Symbol of string
  | Lambda of id list * core_expression
  | Nil
  | Any

and expr =
  | ExprCall of core_expression * string * core_expression list (* receiver, method, args *)
  | ExprFunc of string * id list * core_expression (* name, args, body *)
  | ExprValue of value
  | ExprVar of id
  | ExprConst of id * core_expression
  | ExprIVar of id
  | ExprAssign of string * core_expression
  | ExprIVarAssign of string * core_expression
  | ExprConstAssign of string * core_expression
  | ExprBody of core_expression * core_expression

and id = string * value

and core_expression = {
  expr_desc : expr;
  expr_loc : Location.t;
}

let rec expr_return_value = function
  | ExprVar ((_, value)) | ExprIVar ((_, value)) | ExprConst ((_, value), _) -> value
  | ExprValue (value) -> value
  | ExprCall _ -> Any
  | ExprFunc (_, _, { expr_desc })
  | ExprBody (_, { expr_desc })
  | ExprAssign (_, { expr_desc })
  | ExprIVarAssign (_, { expr_desc })
  | ExprConstAssign (_, { expr_desc }) -> expr_return_value expr_desc
