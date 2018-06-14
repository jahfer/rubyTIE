open Ast
module TypeTree = Disjoint_set

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
  | TFunc of t list * t

let current_var = ref 1
let gen_fresh_t () =
  let tv = !current_var in incr current_var;
  (* Printf.printf "-- Creating new type var t/1%03i\n" tv; *)
  TPoly(Core.sprintf "T%i" tv)

let rec typeof_value = function
  | Hash _   -> THash
  | Bool _   -> TBool
  | Float _  -> TFloat
  | Int _    -> TInt
  | Array _  -> TArray (gen_fresh_t ())
  | String _ -> TString
  | Symbol _ -> TSymbol
  | Nil      -> TNil
  | Any      -> gen_fresh_t ()

type metadata = {
  expr_loc : Location.t;
  type_reference : t TypeTree.t;
  level : int;
}

type 'a expression_type = RawType of t | TypeMetadata of metadata

let rec typeof_expr = let open Ast in function
    | ExprVar ((_, value))
    | ExprIVar ((_, value))
    | ExprConst ((_, value), _)
    | ExprValue (value) -> 
      RawType(typeof_value value)
    | ExprFunc (_, _, (_, metadata))
    | ExprLambda (_, (_, metadata))
    | ExprBlock (_, (_, metadata))
    | ExprAssign (_, (_, metadata))
    | ExprIVarAssign (_, (_, metadata))
    | ExprCall ((_, metadata), _, _)
    | ExprConstAssign (_, (_, metadata)) ->
      TypeMetadata(metadata)
