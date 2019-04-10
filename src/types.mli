module BaseType : sig
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

    val compare : t -> t -> int
end

type t = BaseType.t

module Interface : sig
  module TypeSet : Set.S with type elt = BaseType.t

  module Method : sig
    type t = {
      receiver : BaseType.t;
      method_name : string;
      arguments : TypeSet.t;
    }

    val compare : t -> t -> int
  end

  module MethodSet : Set.S with type elt = Method.t

  type t = {
    methods : MethodSet.t;
    parent : BaseType.t;
    class_name : string;
  }
end

type type_resolution = Unresolved | Resolved

type type_metadata = {
  location : Location.t option;
  binding : string option;
  level : type_resolution;
}

type type_reference = (BaseType.t, type_metadata) Disjoint_set.t

type expr_metadata = {
  expr_loc : Location.t;
  type_reference : type_reference;
  level : int;
}

type 'a expression_type = GeneralizedType of BaseType.t | SpecializedType of type_reference

val typeof_expr : expr_metadata Ast.expr -> 'a expression_type

val gen_fresh_t : unit -> t

val typeof_value : Ast.value -> t
