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
  | Member of string * t (* member name, return value *)
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

(* Printer Utility *)

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
    printf "%a\n" Ast.Printer.print_cexpr core_expr

  let print_constraint k v =
    match v with
    | Literal (t) ->
      Printf.printf "\027[31m[%s CONSTRAINT: Literal (%s)]\027[m\n" k (type_to_str t)
    | Member (member, return_t) ->
      Printf.printf "\027[31m[%s CONSTRAINT: Member (%s.%s -> %s)]\027[m\n" k k member (type_to_str return_t)
    | Equality (a, b) ->
      Printf.printf "\027[31m[%s CONSTRAINT: Equality (%s = %s)]\027[m\n" k (type_to_str a) (type_to_str b)
    | _ ->
      Printf.printf "\027[31m[%s CONSTRAINT: Unknown]\027[m\n" k
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

(* Constraint Generation *)

module ConstraintMap = Map.Make (String)

let append_constraint k c map =
    let lst = match ConstraintMap.find_opt k map with
    | Some(lst) -> lst
    | None -> []
    in map |> ConstraintMap.add k (c :: lst)

let build_constraints constraint_map { expr_desc; expr_type; level } =
  match (level, expr_type) with
  | 0, TPoly (tstr) -> begin match expr_desc with
    | ExprAssign (v, iexpr) | ExprIVarAssign (v, iexpr) | ExprConstAssign (v, iexpr) ->
      let typ = typeof_expr expr_desc in begin
      match typ with
      | TPoly t -> append_constraint t (Equality (typ, expr_type)) constraint_map
      | TLambda (_, (TPoly(t) as pt)) -> constraint_map
        (* TODO: Wrong... *)
        |> append_constraint t (Equality (pt, expr_type))
        |> append_constraint tstr (Literal typ)
      | _ -> append_constraint tstr (Literal typ) constraint_map
      end
    | ExprCall ({ expr_desc }, meth, _args) ->
      let return_typ = typeof_expr expr_desc in
      constraint_map |> append_constraint tstr (Member(meth, return_typ))
    | _ -> constraint_map
  end
  | _ -> constraint_map

(* Annotations *)

module AnnotationMap = Map.Make (String)
let annotations = ref AnnotationMap.empty

let rec create_annotation name =
  (* Printf.printf "-- Looking up annotion for %s...\n" name; *)
  match AnnotationMap.find_opt name !annotations with
  | Some(typ) -> begin (* Printf.printf "-- Found type %s\n" (Printer.type_to_str typ); *) typ end
  | None -> let gen_typ = gen_fresh_t () in
    annotations := AnnotationMap.add name gen_typ !annotations;
    (* Printf.printf "-- Creating type var %s for '%s'\n" (Printer.type_to_str gen_typ) name; *)
    gen_typ

and annotate ({ expr_loc; expr_desc } : core_expression) =
  let typ = match expr_desc with
  | ExprValue (v) -> gen_fresh_t ()
  | ExprCall  ({ expr_desc }, meth, _args) -> begin match expr_desc with
    | ExprValue(Nil) -> create_annotation meth
    | ExprVar(name, _) | ExprIVar(name, _) | ExprConst((name, _), _) -> create_annotation name
    | _ -> begin Printf.printf "!! UNIMPLEMENTED\n"; gen_fresh_t () (* TODO: look up method in object table *) end
    end
  | ExprBody (_, expr) -> let { expr_type } = annotate expr in expr_type
  | ExprVar (name, _) | ExprIVar (name, _) | ExprConst ((name, _), _)
  | ExprFunc (name, _, _) -> create_annotation name
  | ExprAssign (name, expr) | ExprIVarAssign (name, expr) | ExprConstAssign (name, expr) ->
    let { expr_type } = annotate expr in
    (* TODO: this overwrites previous type information *)
    annotations := AnnotationMap.add name expr_type !annotations;
    expr_type
  in { expr_loc; expr_desc; expr_type = typ; level = 0 }

(* AST -> TypedAST *)

let apply_constraints ast constraint_map =
  let _ = constraint_map |> ConstraintMap.iter (fun k vs ->
    vs |> List.iter (fun v -> Printer.print_constraint k v)
  )
  in ast

let rec to_typed_ast core_expr =
  let constraint_map = ConstraintMap.empty in
  let annotations = annotate core_expr in
  let constraints = build_constraints constraint_map annotations in
  apply_constraints annotations constraints


