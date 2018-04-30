open Ast

(* Define data structures and types for AST *)

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

type constraint_t =
  | Literal of t
  | Member of t * string (* TPoly, Member name *)
  | Equality of t * t
  | Disjuction of constraint_t list
  | Overload of t
  | Class of t

type typed_core_expression = {
  expr_desc : expr;
  expr_loc : Location.t;
  expr_type : t;
  level : int;
}

module Printer = struct
  open Core

  let rec type_to_str = function
    | THash    -> "hash"
    | TArray t -> sprintf "array<%s>" (type_to_str t)
    | TString  -> "string"
    | TSymbol  -> "symbol"
    | TInt     -> "int"
    | TFloat   -> "float"
    | TConst t -> sprintf "const<%s>" (type_to_str t)
    | TBool    -> "bool"
    | TNil     -> "nil"
    | TAny     -> "any"
    | TLambda (args, ret)  -> sprintf "lambda<(...) -> %s>" (type_to_str ret)
    | TPoly t -> sprintf "%s" t

  let rec print_cexpr outc ({ expr_loc; expr_desc; expr_type }) =
    (* printf "%a\n" Location.print_loc expr_loc; *)
    let core_expr = { expr_loc; expr_desc } in
    printf " %*s : %a" 6 (type_to_str expr_type) Ast.Printer.print_cexpr core_expr

  let print_constraint k v =
    match v with
    | Literal (t) -> Printf.printf "\027[31m[%s CONSTRAINT: Literal (%s)]\027[m\n" k (type_to_str t)
    | Member (receiver, member) ->
      Printf.printf "\027[31m[%s CONSTRAINT: Member (%s.%s -> %s)]\027[m\n" k (type_to_str receiver) member k
    | _ -> Printf.printf "\027[31m[%s CONSTRAINT: Unknown]\027[m\n" k
end

(* Build map and generator for unique type names *)

let current_var = ref 1
let gen_fresh_t () =
  let tv = !current_var in
  incr current_var; TPoly(Core.sprintf "t/1%03i" tv)

let rec typeof_expr expr =
  let typeof_value = function
    | Hash _   -> THash
    | Bool _   -> TBool
    | Float _  -> TFloat
    | Int _    -> TInt
    | Array _  -> TArray (gen_fresh_t ())
    | String _ -> TString
    | Symbol _ -> TSymbol
    | Lambda (args, { expr_desc }) ->
      let typ = typeof_expr expr_desc in TLambda ([TAny], typ)
    | Nil      -> TNil
    | Any      -> gen_fresh_t ()
  in expr |> expr_return_value |> typeof_value

(* AST -> TypedAST *)

module ConstraintMap = Map.Make (String)

let build_constraints { expr_desc; expr_type } =
  let constraint_map = ConstraintMap.empty in
  match expr_type with
  | TPoly tstr -> begin
    match expr_desc with
    | ExprAssign _ | ExprIVarAssign _ | ExprConstAssign _ ->
      let typ = typeof_expr expr_desc in
      ConstraintMap.add tstr (Literal typ) constraint_map
    | ExprCall ({ expr_desc }, meth, _args) ->
      let typ = typeof_expr expr_desc in
      ConstraintMap.add tstr (Member(typ, meth)) constraint_map
    | _ -> constraint_map
    end
  | _ -> constraint_map

let apply_constraints ast constraint_map =
  let _ = constraint_map |> ConstraintMap.iter (fun k v ->
   Printer.print_constraint k v
  )
  in ast

module AnnotationMap = Map.Make (String)
let annotations = ref AnnotationMap.empty

let rec to_typed_ast core_expr =
  let annotations = annotate core_expr in
  let constraints = build_constraints annotations in
  apply_constraints annotations constraints

and annotate ({ expr_loc; expr_desc } : core_expression) =
  let typ = match expr_desc with
  | ExprValue (v) -> gen_fresh_t ()
  | ExprCall  ({ expr_desc }, meth, _args) -> begin match expr_desc with
    | ExprValue(Nil) -> begin match AnnotationMap.find_opt meth !annotations with
      | Some(typ) -> typ
      | None -> let gen_typ = gen_fresh_t () in
        annotations := AnnotationMap.add meth gen_typ !annotations;
        gen_typ
    end
    | _ -> gen_fresh_t () (* TODO: look up method in object table *)
    end
  | ExprBody (_, expr) -> let { expr_type } = annotate expr in expr_type
  | ExprVar (name, _) | ExprIVar (name, _) | ExprConst ((name, _), _)
  | ExprAssign (name, _) | ExprIVarAssign (name, _) | ExprConstAssign (name, _)
  | ExprFunc (name, _, _) ->
    begin match AnnotationMap.find_opt name !annotations with
      | Some(typ) -> typ
      | None -> let gen_typ = gen_fresh_t () in
        annotations := AnnotationMap.add name gen_typ !annotations;
        gen_typ
    end
  in { expr_loc; expr_desc; expr_type = typ; level = 1 }
