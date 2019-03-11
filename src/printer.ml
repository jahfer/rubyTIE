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
  match root_t.metadata.binding with
  | Some (name) -> name
  | None -> (type_to_str root_t.elem)

let rec print_inheritance (x : type_reference) =
  let deref_parent = !(x.parent) in
  if deref_parent != x then
    Core.sprintf "%s > %s" (print_inheritance !(x.parent)) (print_type_reference x)
  else
    Core.sprintf "[%s {%i}]" (print_type_reference x) !(x.rank)
