open Core

type value =
| Int of int 
| Bool of bool
| Unit of unit
| Prod of value * value
| Left of value 
| Right of value

module Binding = struct
  type t = value
  type error = never_returns
  let unbound _ = failwith "[INTERNAL ERROR]: problem with type-checking" 
end

module Env = Env.Make (Binding)
