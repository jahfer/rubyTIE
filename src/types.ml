open Ast

module BaseType = struct
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
    | TUnion of t * t

  let compare = compare
end

include BaseType

(* Global generator of type variable references *)
let current_var = ref 1
let gen_fresh_t () =
  let tv = !current_var in incr current_var;
  (* Printf.printf "-- Creating new type var t/1%03i\n" tv; *)
  TPoly(Core.sprintf "T%i" tv)

(* Conversion from Literal to Ruby type *)
let typeof_value = function
  | Hash _   -> THash
  | Bool _   -> TBool
  | Float _  -> TFloat
  | Int _    -> TInt
  | Array _  -> TArray (gen_fresh_t ())
  | String _ -> TString
  | Symbol _ -> TSymbol
  | Nil      -> TNil
  | Any      -> gen_fresh_t ()

(* Information stored alongside each type variable *)
type type_resolution = Unresolved | Resolved
type type_metadata = {
  location : Location.t option;
  binding : string option;
  level : type_resolution;
}

(* Structure of type variable *)
type type_reference = (t, type_metadata) Disjoint_set.t

(* Information stored alongside each expression *)
type expr_metadata = {
  expr_loc : Location.t;
  type_reference : type_reference;
  level : int;
}

type 'a expression_type = GeneralizedType of t | SpecializedType of type_reference

let typeof_expr = let open Ast in function
    | ExprVar ((_, value))
    | ExprIVar ((_, value))
    | ExprConst ((_, value), _)
    | ExprValue (value) -> GeneralizedType(typeof_value value)
    | ExprFunc (_, _, (_, metadata))
    | ExprLambda (_, (_, metadata))
    | ExprBlock (_, (_, metadata))
    | ExprAssign (_, (_, metadata))
    | ExprIVarAssign (_, (_, metadata))
    | ExprCall ((_, metadata), _, _)
    | ExprConstAssign (_, (_, metadata)) -> SpecializedType(metadata.type_reference)
