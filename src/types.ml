open Ast
module TypeTree = Disjoint_set

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
type type_reference = (t, type_metadata) TypeTree.t

let has_binding (t : type_reference) = match t.metadata.binding with
| Some _ -> true
| None -> false

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

module BaseTypeMap = Map.Make (BaseType)

let base_type_reference =
  let type_map = BaseTypeMap.empty in
  fun (base_type : BaseType.t) : type_reference ->
    match type_map |> BaseTypeMap.find_opt base_type with
    | Some (type_ref) -> type_ref
    | None -> base_type |> TypeTree.make
      ~root:true
      ~metadata:{ location = None; binding = None; level = Resolved }
