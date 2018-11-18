open Core
open Result
open Types
open Typed

type cause =
| Expected of Typed.t * Typed.t
| Unbound of Var.t
| NotFunction of Typed.t
| NotProd of Typed.t
| NotSum of Typed.t

type error = cause * Span.t

module Binding = struct
  type t = Typed.t
  type error = cause
  let unbound v = Unbound v
end

module Env = Env.Make (Binding)

let rec check_type (t: Type.t) : Typed.t =
  let open Type in match fst t with
  | Int -> Int
  | Bool -> Bool
  | Unit -> Unit
  | Fun (l, r) -> Fun (check_type l, check_type r)
  | Prod (l, r) -> Prod (check_type l, check_type r)
  | Sum (l, r) -> Sum (check_type l, check_type r)

let rec check_exp (e: Exp.t) (env: Env.t) : (Typed.t, error) result =
  let open Exp in match fst e with
  | Int _ -> Ok Int
  | True
  | False -> Ok Bool
  | Unit  -> Ok Unit
  | Var v -> Env.find env v
  | Let (v, e, e') -> check_let v e e' env
  | Abs (v, t, e) -> check_abs v t e env
  | App (e, e') -> check_app e e' env
  | If (b, t, f) -> check_if b t f env
  | Bin (op, l, r) -> check_bin op l r env
  | Uno (op, e) -> check_uno op e env
  | Prod (e, e') -> check_prod e e' env
  | Pil e -> check_pil e env
  | Pir e -> check_pir e env
  | Inl (t, e) -> check_inl t e env
  | Inr (t, e) -> check_inr t e env
  | Case (e, l, r) -> check_case e l r env

and check_let v e e' env =
  check_exp e env
  >>| Env.insert env v
  >>= check_exp e'

and check_abs v t e env =
  let t = check_type t in
  let env' = Env.insert env v t in
  check_exp e env' >>= fun t' -> Ok (Fun (t, t'))

and check_app e e' env =
  check_exp e env >>= fun t ->
  check_exp e' env >>= fun t' ->
  match t with
  | Fun (i, o) when Typed.equal t' i -> Ok o
  | Fun (i, _) -> Error (Expected (i, t'), snd e')
  | _ -> Error (NotFunction t, snd e)

and check_if b t f env =
  check_exp b env >>= fun bt ->
  check_exp t env >>= fun tt ->
  check_exp f env >>= fun ft ->
  match bt with
  | Bool when Typed.equal tt ft -> Ok tt
  | Bool -> Error (Expected (tt, ft), snd f)
  | _ -> Error (Expected (Bool, bt), snd b)

and check_bin op l r env =
  let (i, o) = let open Bin in match fst op with 
  | Add | Sub | Mul | Div -> (Int, Int)
  | LAnd | LOr -> (Bool, Bool)
  | Lt | Le | Ge | Gt | Eq | Ne -> (Int, Bool)
  in
  check_exp l env >>= fun lt ->
  check_exp r env >>= fun rt ->
  if Typed.equal lt i && Typed.equal rt i then
    Ok o
  else if Typed.equal lt i then
    Error (Expected (i, rt), snd r)
  else
    Error (Expected (i, lt), snd l)

and check_uno op e env =
  let (i, o) = let open Uno in match fst op with
  | Neg -> (Int, Int)
  | Not -> (Bool, Bool)
  in
  check_exp e env >>= fun t ->
  if Typed.equal t i then
    Ok o
  else
    Error (Expected (i, t), snd e)

and check_prod e e' env =
  check_exp e env
  >>= fun t -> check_exp e' env
  >>= fun t' -> Ok (Prod (t, t'))

and check_pil e env =
  check_exp e env >>= function
  | Prod (t, _) -> Ok t
  | t -> Error (NotProd t, snd e)

and check_pir e env =
  check_exp e env >>= function
  | Prod (_, t) -> Ok t
  | t -> Error (NotProd t, snd e)

and check_inl t e env =
  begin match check_type t with
  | Sum (l, _) as t' -> Ok (l, t')
  | t' -> Error (NotSum t', snd t)
  end
  >>= fun (lt, st) -> check_exp e env
  >>= fun t ->
  if Typed.equal lt t then
    Ok st
  else
    Error (Expected (lt, t), snd e)

and check_inr t e env =
  begin match check_type t with
  | Sum (_, r) as t' -> Ok (r, t')
  | t' -> Error (NotSum t', snd t)
  end
  >>= fun (rt, st) -> check_exp e env
  >>= fun t ->
  if Typed.equal rt t then
    Ok st
  else
    Error (Expected (rt, t), snd e)

and check_case e l r env =
  let open Typed in
  check_exp e env >>= fun t ->
  check_exp l env >>= fun lt' ->
  check_exp r env >>= fun rt' ->
  match t, lt', rt' with
  | Prod (lt, rt), Fun (li, lo), Fun (ri, ro)
    when equal lt li && equal rt ri && equal lo ro ->
    Ok lo
  | Prod (lt, rt), Fun (li, lo), Fun (_, _)
    when equal lt li ->
    Error (Expected (Fun (rt, lo), rt'), snd r)
  | Prod (lt, _), Fun (_, _), Fun (_, ro) ->
    Error (Expected (Fun (lt, ro), lt'), snd l)
  | _, _, _ -> Error (NotProd t, snd e)
