open Core
open Types

module rec Value : sig 
  type t =
  | Int of int 
  | Bool of bool
  | Unit of unit
  | Prod of t * t
  | Left of t 
  | Right of t
  | Fun of Var.t * Env.t * Exp.t
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

and Env : sig 
  type t = (Var.t * Value.t) list
  val empty: t
  val insert: t -> Var.t -> Value.t -> t
  val find: t -> Var.t -> (Value.t, never_returns * Span.t) result
end = Simple.Env.Make (Binding)

let rec eval_exp e env =
  match e with
  | _ -> failwith "unimplemented"
