open Core
open Types

let impossible () =
  failwith "[INTERNAL ERROR]: problem with type-checking"

module rec Value : sig 
  type t =
  | Int of int 
  | Bool of bool
  | Unit of unit
  | Prod of t * t
  | Left of t 
  | Right of t
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
  let unbound _ = impossible ()
end

and Environment : sig 
  type t = (Var.t * Value.t) list
  val empty: t
  val insert: t -> Var.t -> Value.t -> t
  val find: t -> Var.t -> (Value.t, never_returns * Span.t) result
end = Env.Make (Binding)

let unwrap = function
| Ok v -> v
| Error _ -> impossible () 

let unwrap_int = function
| Value.Int n -> n
| _ -> impossible ()

let unwrap_bool = function
| Value.Bool b -> b
| _ -> impossible ()

let rec eval_exp (e: Exp.t) (env: Environment.t) : Value.t =
  let open Exp in
  match fst e with
  | Int (n, _) -> Int n
  | True -> Bool true
  | False -> Bool false
  | Unit -> Unit ()
  | Var v -> Environment.find env v |> unwrap
  | Let (v, e, e') -> eval_let v e e' env
  | Abs (v, _, e) -> eval_abs v e env
  | App (e, e') -> eval_app e e' env
  | If (b, t, f) -> eval_if b t f env
  | Bin (op, l, r) -> eval_bin op l r env
  | Uno (op, e) -> eval_uno op e env
  | Prod (e, e') -> eval_prod e e' env
  | Pil e -> eval_pil e env
  | Pir e -> eval_pir e env
  | Inl (_, e) -> eval_inl e env
  | Inr (_, e) -> eval_inr e env
  | Case (e, l, r) -> eval_case e l r env

and eval_let v e e' env =
  let env' = Environment.insert env v (eval_exp e env) in
  eval_exp e' env'

and eval_abs v e env =
  Fun (v, env, e)

and eval_app_value e value env =
  match eval_exp e env with
  | Fun (v, fun_env, fun_e) ->
    let fun_env' = Environment.insert fun_env v value in
    eval_exp fun_e fun_env'
  | _ -> impossible ()

and eval_app e e' env =
  let value = eval_exp e' env in
  eval_app_value e value env

and eval_if b t f env =
  match eval_exp b env with
  | Bool true -> eval_exp t env
  | Bool false -> eval_exp f env
  | _ -> impossible ()

and eval_bin op l r env =
  let int_to_int f =
    let l = eval_exp l env |> unwrap_int in
    let r = eval_exp r env |> unwrap_int in
    Value.Int (f l r)
  in
  let bool_to_bool f =
    let l = eval_exp l env |> unwrap_bool in
    let r = eval_exp r env |> unwrap_bool in
    Value.Bool (f l r)
  in
  let int_to_bool f =
    let l = eval_exp l env |> unwrap_int in
    let r = eval_exp r env |> unwrap_int in
    Value.Bool (f l r)
  in
  let open Bin in
  match fst op with
  | Add -> int_to_int ( + )
  | Sub -> int_to_int ( - )
  | Mul -> int_to_int ( * )
  | Div -> int_to_int ( / )
  | LAnd -> bool_to_bool ( && )
  | LOr -> bool_to_bool ( || )
  | Lt -> int_to_bool ( < )
  | Le -> int_to_bool ( <= )
  | Ge -> int_to_bool ( >= )
  | Gt -> int_to_bool ( > )
  | Eq -> int_to_bool ( = )
  | Ne -> int_to_bool ( <> )

and eval_uno op e env =
  let open Uno in
  match fst op with
  | Neg ->
    let n = eval_exp e env |> unwrap_int in
    Value.Int (-n)
  | Not ->
    let b = eval_exp e env |> unwrap_bool in
    Value.Bool (not b)

and eval_prod e e' env =
  Prod (eval_exp e env, eval_exp e' env)

and eval_pil e env =
  match eval_exp e env with
  | Prod (l, _) -> l
  | _ -> impossible ()

and eval_pir e env =
  match eval_exp e env with
  | Prod (_, r) -> r
  | _ -> impossible ()

and eval_inl e env =
  Left (eval_exp e env)

and eval_inr e env =
  Right (eval_exp e env)

and eval_case e l r env =
  match eval_exp e env with
  | Left value -> eval_app_value l value env
  | Right value -> eval_app_value r value env
  | _ -> impossible ()
