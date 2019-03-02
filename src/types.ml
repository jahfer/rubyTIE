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

let current_var = ref 1
let gen_fresh_t () =
  let tv = !current_var in incr current_var;
  (* Printf.printf "-- Creating new type var t/1%03i\n" tv; *)
  TPoly(Core.sprintf "T%i" tv)

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

type type_reference = (t, Location.t) TypeTree.t


module BaseTypeMap = Map.Make (BaseType)

let base_type_reference =
  let type_map = BaseTypeMap.empty in
  fun (base_type : BaseType.t) : type_reference ->
    match type_map |> BaseTypeMap.find_opt base_type with
    | Some (type_ref) -> type_ref
    | None -> TypeTree.make ~root:true None base_type

type metadata = {
  expr_loc : Location.t;
  type_reference : type_reference;
  level : int;
}

type 'a expression_type = RawType of t | TypeMetadata of metadata

let typeof_expr = let open Ast in function
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
