open Ast

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

type typed_core_expression = {
  expr_desc : expr;
  expr_loc : Location.t;
  expr_type : t;
}

let current_var = ref (Char.code 'a')

let gen_fresh_t () =
  let tv = !current_var in
  incr current_var; TPoly(Core.sprintf "'%c" (Char.chr tv))

let typed_expr ({ expr_loc; expr_desc } : core_expression) expr_type =
  { expr_loc; expr_desc; expr_type }

let rec typeof_value = function
  | Hash _   -> THash
  | Bool _   -> TBool
  | Float _  -> TFloat
  | Int _    -> TInt
  | Array _  -> TArray (gen_fresh_t ())
  | String _ -> TString
  | Symbol _ -> TSymbol
  | Lambda (args, body) -> let { expr_type } = eval_types body in TLambda ([TAny], expr_type)
  | Nil      -> TNil
  | Any      -> gen_fresh_t ()

and eval_types (({ expr_desc } as core_expr) : core_expression) =
  let as_typed_expr = typed_expr core_expr in
  match expr_desc with
  | ExprCall _ -> (* should look up method in table *) gen_fresh_t () |> as_typed_expr
  | ExprValue (value) -> value |> typeof_value |> as_typed_expr
  | ExprVar ((_, value))
  | ExprIVar ((_, value)) -> value |> typeof_value |> as_typed_expr
  | ExprConst ((_, value), _) -> TConst (typeof_value value) |> as_typed_expr
  | ExprAssign (_, typed_expr)
  | ExprIVarAssign (_, typed_expr)
  | ExprConstAssign (_, typed_expr) ->
    let { expr_type } = eval_types typed_expr in expr_type |> as_typed_expr
  | ExprBody (_, expr) ->
    let { expr_type } = eval_types expr in expr_type |> as_typed_expr
  | ExprFunc (_, _, expr) ->
    let { expr_type } = eval_types expr in expr_type |> as_typed_expr

let eval core_expr = eval_types core_expr
