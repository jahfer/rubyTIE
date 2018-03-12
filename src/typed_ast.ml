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

and eval_types ({ expr_loc; expr_desc } : core_expression) =
  let typ = match expr_desc with
    | ExprCall _ -> (* TODO: look up method in table *) gen_fresh_t ()
    | ExprConst ((_, value), _) -> TConst (typeof_value value)
    | ExprValue (value)    | ExprVar ((_, value)) | ExprIVar ((_, value)) -> typeof_value value
    | ExprAssign (_, expr) | ExprIVarAssign (_, expr) | ExprConstAssign (_, expr)
    | ExprBody (_, expr)   | ExprFunc (_, _, expr) ->
      let { expr_type } = eval_types expr in expr_type
  in { expr_loc; expr_desc; expr_type = typ }

let eval core_expr = eval_types core_expr
