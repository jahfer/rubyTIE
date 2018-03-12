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
  level : int;
}

let current_var = ref 1
let gen_fresh_t () =
  let tv = !current_var in
  incr current_var; TPoly(Core.sprintf "v/1%03i" tv)

module AnnotationMap = Map.Make (String)
let annotations = AnnotationMap.empty

let rec eval core_expr = core_expr |> annotate (* |> constrain_types *)

and typeof_value = function
  | Hash _   -> THash
  | Bool _   -> TBool
  | Float _  -> TFloat
  | Int _    -> TInt
  | Array _  -> TArray (gen_fresh_t ())
  | String _ -> TString
  | Symbol _ -> TSymbol
  | Lambda (args, body) ->
    let { expr_type } = eval body in TLambda ([TAny], expr_type)
  | Nil      -> TNil
  | Any      -> gen_fresh_t ()

and constrain_types { expr_loc; expr_desc; expr_type } =
  let typ = match expr_desc with
    | ExprCall _ -> (* TODO: look up method in table *) gen_fresh_t ()
    | ExprConst ((_, value), _) -> TConst (typeof_value value)
    | ExprValue (value)    | ExprVar ((_, value))     | ExprIVar ((_, value)) -> typeof_value value
    | ExprAssign (_, expr) | ExprIVarAssign (_, expr) | ExprConstAssign (_, expr)
    | ExprBody (_, expr)   | ExprFunc (_, _, expr) ->
      let { expr_type } = eval expr in expr_type
  in { expr_loc; expr_desc; expr_type = typ; level = 2 }

and annotate ({ expr_loc; expr_desc } : core_expression) : typed_core_expression =
  let typ = match expr_desc with
  | ExprVar (name, _) | ExprIVar (name, _) | ExprConst ((name, _), _)
  | ExprAssign (name, _) | ExprIVarAssign (name, _) | ExprConstAssign (name, _)
  | ExprFunc (name, _, _) ->
    begin match AnnotationMap.find_opt name annotations with
      | Some(typ) -> typ
      | None -> gen_fresh_t ()
    end
  | ExprCall ({ expr_desc } as receiver, meth, _) ->
    begin match expr_desc with
      | ExprVar (name, _) | ExprIVar (name, _) | ExprConst ((name, _), _) ->
      begin match AnnotationMap.find_opt name annotations with
        | Some(typ) -> typ
        | None -> gen_fresh_t ()
      end
      | _ -> let { expr_type } = annotate receiver in expr_type
    end
  | _ -> TAny
  in { expr_loc; expr_desc; expr_type = typ; level = 1 }
