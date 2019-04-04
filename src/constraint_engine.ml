open Types
open Ast

exception TypeError of type_reference * type_reference
(* exception AssignmentError of expression * expression *)

module Constraints = struct
  type t =
    | Literal of type_reference * Types.t
    (* member name, args, receiver, return type *)
    | Method of string * type_reference list * type_reference * type_reference
    (* name, args, return type *)
    | FunctionApplication of string * type_reference list * type_reference
    | Equality of type_reference * type_reference
    | Disjuction of t list
    | Overload of type_reference
    | Class of type_reference
    | SubType of type_reference * type_reference

  let compare a b = match a, b with
    (* Literals are always sorted first *)
    | Literal _, _ -> -1 | _, Literal _ -> 1
    (* SubType is second highest priority *)
    | SubType _, _ -> -1 | _, SubType _ -> 1
    | a, b -> Pervasives.compare a b
end

type constraint_t = Constraints.t

module ConstraintMap = Map.Make (String)

type constraint_map_t = constraint_t list ConstraintMap.t

module BaseTypeMap = Map.Make (Types.BaseType)

(** Looks up type_reference for literal type. If not found,
 ** it will produce a type_reference and store it internally
 ** for next lookup. *)
let dynamic_type_lookup =
  fun () -> let type_map = ref BaseTypeMap.empty in
  fun (base_type : BaseType.t) : type_reference ->
    match !type_map |> BaseTypeMap.find_opt base_type with
    | Some (type_ref) -> type_ref
    | None ->
      let t = base_type
      |> TypeTree.make ~root:true ~metadata:{
        location = None;
        binding = None;
        level = Resolved
      } in
      type_map := BaseTypeMap.add base_type t !type_map;
      t

let reference_table : (string, type_reference) Hashtbl.t = Hashtbl.create 1000

let unify_types a b =
  let open TypeTree in
  try union a b with Incompatible_nodes ->
    let union_t = TUnion((find a).elem, (find b).elem) in
    let union_t_node = TypeTree.make ~root:true ~metadata:a.metadata union_t in
    replace_root a union_t_node

let unify_types_exn a b = TypeTree.union a b

let append_constraint
  (k : string)
  (c : Constraints.t)
  (map : Constraints.t list ConstraintMap.t) =
    let lst = match ConstraintMap.find_opt k map with
    | Some(lst) -> lst
    | None -> []
    in map |> ConstraintMap.add k (c :: lst)

let find_or_insert name t tbl =
  if Hashtbl.mem tbl name
  then Some(Hashtbl.find tbl name)
  else (Hashtbl.add tbl name t; None)

(*
  After constraints have been found, iterate over constraints
  until definition for all types can be found.
*)
let simplify base_type_reference constraint_lst =
  let reducer lst cst =
    let action = match cst with
    | Constraints.Literal(ref, t) ->
      (* type variable is a literal, unify with literal type reference *)
      let base_reference = base_type_reference t in
      unify_types ref base_reference;
      None
    | Constraints.SubType(supertype, subtype) as st ->
      (* Types are pointing to the same variable, unify and drop constraint *)
      if has_binding supertype &&
        supertype.metadata.binding = subtype.metadata.binding
      then (unify_types supertype subtype; None)
      (* supertype is a labeled variable or resolved type, keep constraint *)
      else if Util.is_some supertype.metadata.binding ||
        (TypeTree.find supertype).metadata.level = Resolved
      then Some(st)
      (* Hierarchy constraint, build relationship and drop constraint *)
      else (supertype.parent := subtype; None)
    | _ -> Some(cst)
    in
    match action with
    | Some hd -> begin match hd with
      | Constraints.SubType(supertype, subtype) ->
        if Util.is_some (TypeTree.find subtype).metadata.binding
        then hd :: lst
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

let simplify_map (constraint_map : constraint_map_t) : constraint_map_t =
  let base_type_reference = dynamic_type_lookup () in
  let (_eliminated_types, constrained_types) = constraint_map
    |> ConstraintMap.mapi(
      fun (_key : string) (constraint_lst : constraint_t list) ->
        simplify base_type_reference constraint_lst
    )
    |> ConstraintMap.partition (
      fun _k v -> (List.compare_length_with v 0) = 0
    )
  in constrained_types

module ConstraintSet = Set.Make (Constraints)

let unify_constraints (nodes : (type_reference * type_reference list) list) : type_reference list =
  let base_type_reference = dynamic_type_lookup () in
  let rec unify_constraints_for_subtype (subtype, supertypes) : type_reference =
    let rec unify_all supertypes : type_reference =
      print_string "Unifying types: ";
      print_endline @@ String.concat ", " @@ List.map Printer.print_type_reference supertypes;
      match supertypes with
      | x :: y :: tail ->
        begin match TypeTree.((find x).elem, (find y).elem) with
        | TPoly(_), _ ->
          Printf.printf "Resolving type dependency: %s\n" @@ Printer.print_type_reference x;
          (* Resolve dependencies for polymorphic type variable *)
          let _ = unify_constraints_for_subtype (x, (List.assq x nodes)) in
          unify_all supertypes
        | _, TPoly(_) ->
          Printf.printf "Resolving type dependency: %s\n" @@ Printer.print_type_reference y;
          (* Resolve dependencies for polymorphic type variable *)
          let _ = unify_constraints_for_subtype (y, (List.assq y nodes)) in
          unify_all supertypes
        | _ ->
          Printf.printf "Unifying base types: %s, %s\n" (Printer.print_type_reference x) (Printer.print_type_reference y);
          (* Build union of base types *)
          let t = TUnion((TypeTree.find x).elem, (TypeTree.find y).elem) in
          let t_ref = TypeTree.make ~root:true ~metadata:x.metadata t in
          print_endline @@ (Printer.print_type_reference t_ref);
          unify_all (t_ref :: tail)
        end
      | x :: [] -> Printf.printf "✅  Only one dependency: %s\n" (Printer.print_type_reference x); x
      | [] -> base_type_reference TAny
    in let union_t = unify_all supertypes
    in let () = unify_types subtype union_t
    in union_t
  in List.map unify_constraints_for_subtype nodes

let solve
  (constraint_map : constraint_map_t) =
  constraint_map
  |> ConstraintMap.bindings
  |> List.map (fun (_, v) -> v)
  |> List.flatten
  (* nodes : a list of (subtype, supertype list) *)
  |> List.fold_left (fun nodes -> function
    | Constraints.SubType (supertype, subtype) ->
      let supertype = TypeTree.find supertype in
      let subtype = TypeTree.find subtype in
      begin
      match List.assq_opt subtype nodes with
      | Some (parents) ->
        (subtype, supertype :: parents) :: (List.remove_assq subtype nodes)
      | None -> (subtype, supertype :: []) :: nodes
      end
    | _ -> nodes
  ) []
  |> unify_constraints
  (* |> List.iter(fun t -> print_endline @@ Printer.print_type_reference t) *)

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

let rec build_constraints constraint_map (expr, { type_reference; _ }) =

  let rec build_constraint type_var_name = function

    | ReferenceConstraint (name) ->
      let opt_existing_type = reference_table
      |> find_or_insert name type_reference in

      begin match opt_existing_type with
        | Some (existing_type) ->
          constraint_map
          |> append_constraint type_var_name @@ SubType (existing_type, type_reference)
        | None -> constraint_map
      end

    | AssignmentConstraint (name, iexpr) ->
      begin match typeof_expr expr with
        | GeneralizedType _ -> constraint_map
        | SpecializedType (expr_type) ->
          let constraint_map =
            ReferenceConstraint name
            |> build_constraint type_var_name
            |> Util.flip build_constraints iexpr in
          match expr_type.elem with
          | TPoly t ->
            constraint_map
            |> append_constraint t @@ SubType (expr_type, type_reference)
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
        in begin
          match (expr_to_constraint_category receiver_expr) with
          | ReferenceConstraint (receiver_name) ->
            let method_name = String.concat "" [ receiver_name; "#"; meth ] in
            build_constraint type_var_name @@ ReferenceConstraint method_name
          | _ -> constraint_map
        end
        |> Util.flip build_constraints receiver_expression
        |> append_constraint type_var_name @@ Method (meth, arg_types, receiver_type, type_reference)
        |> Util.flip (List.fold_left build_constraints) args

      (* Lambda definition *)
      | ExprLambda (_, expression) ->
        let (_, {type_reference = return_type_node; _}) = expression in
        let return_type = (TypeTree.find return_type_node).elem in
        let lambda_type = TLambda([TAny], return_type) in
        build_constraints constraint_map expression
        |> append_constraint type_var_name @@ Literal (type_reference, lambda_type)

      | _ -> constraint_map
  in
  match type_reference.elem with
  | TPoly (type_var_name) ->
    expr_to_constraint_category expr
    |> build_constraint type_var_name
  | _ -> constraint_map
