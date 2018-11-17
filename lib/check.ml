open Core
open Result
open Types

type error =
| Expected of (Typed.t * (Typed.t * Span.t))
| Unbound of Var.t

module Env = struct
  type t = (Var.t * Typed.t) list

  let empty = []

  let insert env v t =
    (v, t) :: env

  let find env v =
    let eq (v', _) = (fst v = fst v') in
    match List.find ~f:eq env with
    | None -> Error (Unbound v)
    | Some (_, t) -> Ok t
end

let check_exp (env: Env.t) (e: Exp.t) : (Typed.t, error) result =
  let open Exp in
  match fst e with
  | Int _ -> Ok Typed.Int
  | True
  | False -> Ok Typed.Bool
  | Unit  -> Ok Typed.Unit
  | Var v -> Env.find env v
  | _ -> failwith "unimplemented"
