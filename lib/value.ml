open Core
open Types

module rec Value : sig 
  type t =
  | Int of int 
  | Bool of bool
  | Unit
  | Prod of t * t
  | Left of Typed.t * t 
  | Right of Typed.t * t
  | Fun of Var.t * Environment.t * Exp.t
end = struct
  include Value
end

and Binding : sig
  type t = Value.t
  type error = never_returns
  val unbound: Var.t -> error
end = struct
  include Binding
  let unbound _ = failwith "[INTERNAL ERROR]: problem with type-checking"
end

and Environment : sig 
  type t = (Var.t * Value.t) list
  val empty: t
  val insert: t -> Var.t -> Value.t -> t
  val find: t -> Var.t -> (Value.t, never_returns * Span.t) result
end = Env.Make (Binding)
