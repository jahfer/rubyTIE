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
  | TFunc of t list * t

type constraint_t =
  | Literal of t
  | Member of string * t (* member name, return value *)
  | Equality of t * t
  | Disjuction of constraint_t list
  | Overload of t
  | Class of t

type metadata = {
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
    | TLambda (args, ret) -> sprintf "lambda<(...) -> %s>" (type_to_str ret)
    | TFunc (args, ret) -> sprintf "func<(...) -> %s>" (type_to_str ret)
    | TPoly t -> sprintf "%s" t

  let rec print_cexpr outc (expr) =
    let { expr_loc; expr_type; level } = expr_metadata expr in
    (* printf "%a\n" Location.print_loc expr_loc; *)
    printf "%s [i%i] : %a\n" (type_to_str expr_type) level Ast.Printer.print_cexpr expr

  let print_constraint k v =
    match v with
    | Literal (t) ->
      Printf.printf "\027[31m[CONSTRAINT: Literal (%s = %s)]\027[m\n" k (type_to_str t)
    | Member (member, return_t) ->
      Printf.printf "\027[31m[CONSTRAINT: Member (%s.%s -> %s)]\027[m\n" k member (type_to_str return_t)
    | Equality (a, b) ->
      Printf.printf "\027[31m[CONSTRAINT: Equality (%s = %s)]\027[m\n" (type_to_str a) (type_to_str b)
    | _ ->
      Printf.printf "\027[31m[CONSTRAINT: %s => Unknown]\027[m\n" k
end

(* Build map and generator for unique type names *)

let current_var = ref 1
let gen_fresh_t () =
  let tv = !current_var in incr current_var;
  TPoly(Core.sprintf "t/1%03i" tv)


let rec typeof_value = function
  | Hash _   -> THash
  | Bool _   -> TBool
  | Float _  -> TFloat
  | Int _    -> TInt
  | Array _  -> TArray (gen_fresh_t ())
  | String _ -> TString
  | Symbol _ -> TSymbol
  | Lambda (args(*, expr*)) ->
    let typ = (*typeof_expr expr*) TAny in TLambda ([TAny], typ)
  | Nil      -> TNil
  | Any      -> gen_fresh_t ()

and typeof_expr expr = expr |> expr_return_value |> typeof_value

(* Constraint Generation *)

module ConstraintMap = Map.Make (String)

let append_constraint k c map =
    let lst = match ConstraintMap.find_opt k map with
    | Some(lst) -> lst
    | None -> []
    in map |> ConstraintMap.add k (c :: lst)

let build_constraints constraint_map (expr, { expr_type; level }) =
  match (level, expr_type) with
  | 0, TPoly (tstr) -> begin match expr with
    | ExprValue(v) -> constraint_map |> append_constraint tstr (Literal (typeof_value v))
    | ExprAssign (v, iexpr) | ExprIVarAssign (v, iexpr) | ExprConstAssign (v, iexpr) ->
      let typ = typeof_expr expr in begin
      match typ with
      | TPoly t -> append_constraint t (Equality (typ, expr_type)) constraint_map
      | TLambda (_, (TPoly(t) as poly_t)) -> constraint_map
        |> append_constraint t (Equality (poly_t, expr_type))
        |> append_constraint tstr (Literal typ)
      | _ -> append_constraint tstr (Literal typ) constraint_map
      end
    | ExprCall ((receiver_expr, _), meth, _args) ->
      let return_typ = typeof_expr receiver_expr in
      constraint_map |> append_constraint tstr (Member(meth, return_typ))
    | _ -> constraint_map
  end
  | _ -> constraint_map

(* Annotations *)

module AnnotationMap = Map.Make (String)
let annotations = ref AnnotationMap.empty

let add_annotation name typ map =
  let typ_name = (Printer.type_to_str typ) in
  Printf.printf "\027[31m[ANNOTATION: Bind ('%s' = %s)]\027[m\n" name typ_name;
  AnnotationMap.add name typ map

let rec create_annotation name =
  match AnnotationMap.find_opt name !annotations with
  | Some(typ) -> typ
  | None -> let gen_typ = gen_fresh_t () in
    annotations := add_annotation name gen_typ !annotations;
    gen_typ

and annotate (expression : Location.t Ast.expression) =
  let rec annotate_expression expr location_meta = begin
    let t = match expr with
      | ExprValue (v) -> gen_fresh_t ()
      | _ -> gen_fresh_t ()
    in ((expr, { expr_loc = location_meta; expr_type = t; level = 0 }) : metadata Ast.expression)
    end
  in let (expr, location_meta) = expression in
  replace_metadata annotate_expression expr location_meta

  (* let typ = match expr with
  | ExprValue (v) -> gen_fresh_t ()
  | ExprCall  ((expr, _), meth, _args) -> begin match expr with
    | ExprVar(name, _) | ExprIVar(name, _) | ExprConst((name, _), _) ->
      let key = String.concat "#" [name; meth] in
      create_annotation key
    | ExprValue(Nil) | _ -> (*create_annotation meth*) begin
      match AnnotationMap.find_opt meth !annotations with
      | Some(TFunc(_, return_t)) -> return_t
      | Some(t) -> t
      | None -> create_annotation meth
      end
    end
  | ExprBody (_, (expr, _)) -> let { expr_type } = expr_metadata (annotate expr) in expr_type
  | ExprVar (name, _) | ExprIVar (name, _) | ExprConst ((name, _), _) -> create_annotation name
  | ExprFunc (name, args, body) ->
    let { expr_type } = expr_metadata (annotate body) in
    let t = TFunc([TAny], expr_type) in
    annotations := add_annotation name t !annotations;
    t
  | ExprAssign (name, expr) | ExprIVarAssign (name, expr) | ExprConstAssign (name, expr) ->
    let { expr_type } = expr_metadata (annotate expr) in
    (* TODO: this overwrites previous type information *)
    annotations := add_annotation name expr_type !annotations;
    expr_type
  in (expr, { expr_loc = meta; expr_type = typ; level = 0 }) *)

(* AST -> TypedAST *)

let apply_constraints ast constraint_map =
  let _ = constraint_map |> ConstraintMap.iter (fun k vs ->
    vs |> List.iter (fun v -> Printer.print_constraint k v)
  ) in ast

let rec to_typed_ast core_expr =
  let constraint_map = ConstraintMap.empty in
  let annotations = annotate core_expr in
  let constraints = build_constraints constraint_map annotations in
  apply_constraints annotations constraints


