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
  | TLambda (args, ret) -> 
    Core.sprintf "lambda<args -> %s>" (type_to_str ret)
  | TFunc (args, ret) ->
    Core.sprintf "func<args -> %s>" (type_to_str ret)
  | TPoly t ->
    Core.sprintf "%s" t
