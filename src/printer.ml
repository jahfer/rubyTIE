open Types

let rec type_to_str = function
  | THash    -> "hash"
  | TString  -> "string"
  | TSymbol  -> "symbol"
  | TInt     -> "int"
  | TFloat   -> "float"
  | TBool    -> "bool"
  | TNil     -> "nil"
  | TAny     -> "any"
  | TConst t ->
    Core.sprintf "const<%s>" (type_to_str t)
  | TArray t ->
    Core.sprintf "array<%s>" (type_to_str t)
  | TLambda (_args, ret) ->
    Core.sprintf "lambda<? -> %s>" (type_to_str ret)
  | TPoly t ->
    Core.sprintf "%s" t
  | TUnion (t1, t2) -> Core.sprintf "%s|%s" (type_to_str t1) (type_to_str t2)

let print_type_reference (t : type_reference) =
  let root_t = TypeTree.find t in
  type_to_str root_t.elem
  (* match root_t.metadata.binding with
  | Some (name) -> Printf.sprintf "%s" (type_to_str root_t.elem) name
  | None -> (type_to_str root_t.elem) *)

let print_type_error (a : Types.type_reference) (b : Types.type_reference) =
  Printf.printf "-- TYPE ERROR %s\n\n" (String.make 40 '-');
  Printf.printf "Type `%s` is not compatible with type `%s`\n"
    (type_to_str (Disjoint_set.find a).elem)
    (type_to_str (Disjoint_set.find b).elem);
  let () = match b.metadata.location with
    | Some (loc) ->
      Location.print_loc loc;
      Printf.printf " type is initialized as `%s` here\n" (type_to_str (Disjoint_set.find b).elem);
    | None -> ()
  in
  match a.metadata.location with
  | Some (loc) ->
    Location.print_loc loc;
    Printf.printf " used as `%s` here\n" (type_to_str (Disjoint_set.find a).elem)
  | None -> ()
