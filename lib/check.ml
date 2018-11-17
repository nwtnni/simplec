open Core
open Result
open Types
open Typed

type cause =
| Expected of Typed.t * Typed.t
| Unbound of Var.t

type error = cause * Span.t

module Env = struct
  type t = (Var.t * Typed.t) list

  let empty = []

  let insert env v t =
    (v, t) :: env

  let find env v =
    let eq (v', _) = (fst v = fst v') in
    match List.find ~f:eq env with
    | None -> Error (Unbound v, snd v)
    | Some (_, t) -> Ok t
end

let rec check_type (t: Type.t) : Typed.t =
  let open Type in match fst t with
  | Int -> Int
  | Bool -> Bool
  | Unit -> Unit
  | Fun (l, r) -> Fun (check_type l, check_type r)
  | Prod (l, r) -> Prod (check_type l, check_type r)
  | Sum (l, r) -> Sum (check_type l, check_type r)

let check_bin (op: Bin.t) : Typed.t =
  let open Bin in match fst op with
  | Add
  | Sub
  | Mul
  | Div -> Fun (Int, Fun (Int, Int))
  | LAnd
  | LOr -> Fun (Bool, Fun (Bool, Bool))
  | Lt
  | Le
  | Ge
  | Gt
  | Eq
  | Ne -> Fun (Int, Fun (Int, Bool))

let check_uno (op: Uno.t) : Typed.t =
  let open Uno in match fst op with
  | Neg -> Fun (Int, Int)
  | Not -> Fun (Bool, Bool)

let rec check_exp (env: Env.t) (e: Exp.t) : (Typed.t, error) result =
  let open Exp in match fst e with
  | Int _ -> Ok Int
  | True
  | False -> Ok Bool
  | Unit  -> Ok Unit
  | Var v -> Env.find env v
  | Let (v, e, e') -> check_let env v e e'
  | _ -> failwith "unimplemented"

and check_let _ _ _ =
  (* match t with *)
  (* | Some t -> *)
  (*   check_e *)
  (* | None -> *)
  failwith "unimplemented"
