open Types
open Ast

(* Exceptions *)

exception TypeError of type_reference * type_reference

(* Constraint type definitions *)

module Constraints = struct
  type t =
    | Literal of type_reference * Types.t
    | SubType of type_reference * type_reference
    (* member name, args, receiver, return type *)
    | Method of string * type_reference list * type_reference * type_reference
    (* name, args, return type *)
    (* | FunctionApplication of string * type_reference list * type_reference
    | Equality of type_reference * type_reference
    | Disjuction of t list
    | Overload of type_reference
    | Class of type_reference *)

  let compare a b = match a, b with
    (* Literals are always sorted first *)
    | Literal _, _ -> -1 | _, Literal _ -> 1
    (* SubType is second highest priority *)
    | SubType _, _ -> -1 | _, SubType _ -> 1
    | a, b -> Pervasives.compare a b

  module Map = Map.Make (String)
  type map_t = t list Map.t
end

module BaseTypeMap = Map.Make (Types.BaseType)

(** Look up type_reference for literal type. If not found, it will
    produce a root type_reference and store it internally for next
    lookup. *)
let base_type_cache =
  fun () -> let type_map = ref BaseTypeMap.empty in
  fun (base_type : BaseType.t) : type_reference ->
    match !type_map |> BaseTypeMap.find_opt base_type with
    | Some (type_ref) -> type_ref
    | None ->
      let t = base_type
      |> Disjoint_set.make ~root:true ~metadata:{
        location = None;
        binding = None;
        level = Resolved
      } in
      type_map := BaseTypeMap.add base_type t !type_map;
      t

(**  *)
let unify_types a b =
  let open Disjoint_set in
  try union a b with Incompatible_nodes ->
    let union_t = TUnion((find a).elem, (find b).elem) in
    let union_t_node = Disjoint_set.make ~root:true ~metadata:a.metadata union_t in
    replace_root a union_t_node

(** After constraints have been found, iterate over constraints
   until definition for all types can be found. *)
let simplify_type_constraints base_type_reference constraint_lst =
  let reducer lst cst =
    let action = match cst with
    | Constraints.Literal (ref, t) ->
      (* type variable is a literal, unify with literal type reference *)
      let base_reference = base_type_reference t in
      Disjoint_set.union ref base_reference;
      None
    | Constraints.SubType (subtype, supertype) as st ->
      (* types are pointing to the same variable, unify and drop constraint *)
      if Util.is_some supertype.metadata.binding &&
        supertype.metadata.binding = subtype.metadata.binding
      then (unify_types supertype subtype; None)
      else Some(st)
    | _ -> Some(cst)
    in
    match action with
    | Some hd -> begin match hd with
      | Constraints.SubType (subtype, supertype) ->
        if Util.is_some (Disjoint_set.find subtype).metadata.binding
        then hd :: lst
        (* subtype is anonymous, unification is safe as there's only one relationship *)
        else begin
          unify_types supertype subtype;
          lst
        end
      | _ -> hd :: lst
      end
    | None -> lst
  in
  constraint_lst
  |> List.sort Constraints.compare
  |> List.fold_left reducer []

(** Given a constraint map, reduce constraints within to simpler versions *)
let simplify (constraint_map : Constraints.map_t) : Constraints.map_t =
  let type_cache = base_type_cache () in
  let (_eliminated_types, constrained_types) = constraint_map
    |> Constraints.Map.mapi(
      fun
        (_key : string)
        (constraint_lst : Constraints.t list) ->
          simplify_type_constraints type_cache constraint_lst
    )
    |> Constraints.Map.partition (
      fun _k v -> (List.compare_length_with v 0) = 0
    )
  in constrained_types

(** Given a list of types and their supertypes, generate final resolved types *)
let unify_constraints nodes : type_reference list =

  let type_cache = base_type_cache () in

  (* TODO: Dependency resolution should only happen once...does it? *)
  let rec unify_constraints_for_subtype
    ((subtype, supertypes) : type_reference * type_reference list) =

    let rec create_union_t = function
    | [] -> type_cache TAny
    | (x :: []) -> x
    | (x :: y :: tail) as supertypes ->
      let resolve_type t =
        let _ = unify_constraints_for_subtype (t, List.assq t nodes) in
        create_union_t supertypes
      in
      match Disjoint_set.((find x).elem, (find y).elem) with
      | TPoly(_), _ -> resolve_type x
      | _, TPoly(_) -> resolve_type y
      | _ ->
        let x' = Disjoint_set.find x
        and y' = Disjoint_set.find y in
        let t = TUnion(x'.elem, y'.elem)
        |> Disjoint_set.make ~root:true ~metadata:{ x.metadata with level = Resolved } in
        create_union_t (t :: tail)
    in let union_t = create_union_t supertypes
    in let () = unify_types subtype union_t
    in union_t
  in List.map unify_constraints_for_subtype nodes

(** Given a set of constraints, derive final types *)
let solve
  (constraint_map : Constraints.map_t) =
  constraint_map
  |> Constraints.Map.bindings
  |> List.map (fun (_, v) -> v)
  |> List.flatten
  (* nodes : a list of (subtype, supertype list) *)
  |> List.fold_left (fun nodes -> function
    | Constraints.SubType (subtype, supertype) ->
      let supertype = Disjoint_set.find supertype in
      let subtype = Disjoint_set.find subtype in
      begin
      match List.assq_opt subtype nodes with
      | Some (parents) ->
        (subtype, supertype :: parents) :: (List.remove_assq subtype nodes)
      | None -> (subtype, supertype :: []) :: nodes
      end
    | _ -> nodes
  ) []
  |> unify_constraints

(* Build constraints from AST *)

type constraint_category =
  | ReferenceConstraint of string
  | AssignmentConstraint of string * expr_metadata expression
  | OtherConstraint of expr_metadata expr

let expr_to_constraint_category e = match e with
  | ExprVar(name, _)
  | ExprIVar(name, _)
  | ExprConst((name, _), _) -> ReferenceConstraint name
  | ExprAssign (name, expr)
  | ExprIVarAssign (name, expr)
  | ExprConstAssign (name, expr) -> AssignmentConstraint (name, expr)
  | ExprCall _
  | ExprLambda _
  | ExprValue _
  | ExprFunc _
  | ExprBlock _ -> OtherConstraint e

let append_constraint
  (k : string)
  (c : Constraints.t)
  (map : Constraints.t list Constraints.Map.t) =
    let lst = match Constraints.Map.find_opt k map with
    | Some(lst) -> lst
    | None -> []
    in map |> Constraints.Map.add k (c :: lst)

let find_or_insert name t tbl =
  if Hashtbl.mem tbl name
  then Some(Hashtbl.find tbl name)
  else (Hashtbl.add tbl name t; None)

let reference_table : (string, type_reference) Hashtbl.t = Hashtbl.create 1000

(** Given an untyped AST, generate constraints based on node interactions *)

let rec build_constraints (expr, { type_reference; _ }) constraint_map =

  let rec build_constraint type_var_name = function

    | ReferenceConstraint (name) ->
      let opt_existing_type = reference_table
      |> find_or_insert name type_reference in

      begin match opt_existing_type with
        | Some (existing_type) ->
          constraint_map
          |> append_constraint type_var_name @@ SubType (type_reference, existing_type)
        | None -> constraint_map
      end

    | AssignmentConstraint (name, iexpr) ->
      begin match typeof_expr expr with
        | GeneralizedType _ -> constraint_map
        | SpecializedType (expr_type) ->
          let constraint_map =
            ReferenceConstraint name
            |> build_constraint type_var_name
            |> build_constraints iexpr in
          match expr_type.elem with
          | TPoly t ->
            constraint_map
            |> append_constraint t @@ SubType (type_reference, expr_type)
          | _ -> constraint_map
      end

    | OtherConstraint (e) -> match e with

      (* Direct Ruby literal reference *)
      | ExprValue (value) ->
        constraint_map
        |> append_constraint type_var_name @@ Literal (type_reference, typeof_value value)

      (* Method invocation *)
      | ExprCall (receiver_expression, meth, args) ->
        let (receiver_expr, {type_reference = receiver_type; _}) = receiver_expression
        and arg_types = args
          |> List.map (fun (_, {type_reference; _}) -> type_reference)
        in
        let map = begin
          match (expr_to_constraint_category receiver_expr) with
          | ReferenceConstraint (receiver_name) ->
            let method_name = String.concat "" [ receiver_name; "#"; meth ] in
            build_constraint type_var_name @@ ReferenceConstraint method_name
          | _ -> constraint_map
        end
        |> build_constraints receiver_expression
        |> append_constraint type_var_name @@ Method (meth, arg_types, receiver_type, type_reference)
        in List.fold_left (Util.flip build_constraints) map args

      (* Lambda definition *)
      | ExprLambda (_, expression) ->
        let (_, {type_reference = return_type_node; _}) = expression in
        let return_type = (Disjoint_set.find return_type_node).elem in
        let lambda_type = TLambda([TAny], return_type) in
        build_constraints expression constraint_map
        |> append_constraint type_var_name @@ Literal (type_reference, lambda_type)

      | _ -> constraint_map
  in
  match type_reference.elem with
  | TPoly (type_var_name) ->
    expr_to_constraint_category expr
    |> build_constraint type_var_name
  | _ -> constraint_map
