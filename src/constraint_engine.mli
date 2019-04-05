exception TypeError of Types.type_reference * Types.type_reference

module Constraints : sig
  type t =
    | Literal of Types.type_reference * Types.t
    (* supertype, subtype *)
    | SubType of Types.type_reference * Types.type_reference
    (* member name, args, receiver, return type *)
    | Method of string * Types.type_reference list * Types.type_reference * Types.type_reference

  val compare : t -> t -> int
  module Map : Map.S with type key = string

  type map_t = t list Map.t
end

val base_type_cache : unit -> Types.BaseType.t -> Types.type_reference

val build_constraints : Types.expr_metadata Ast.expression -> Constraints.map_t -> Constraints.map_t

val simplify : Constraints.map_t -> Constraints.map_t

val solve : Constraints.map_t -> Types.type_reference list
