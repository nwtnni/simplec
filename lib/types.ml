type 'a span = 'a * Span.t

module Int = struct
  type t = int span
end

module Var = struct
  type t = string span
end

module String = struct
  type t = string span
end

module Type = struct
  type pre_t =
  | Int
  | String
  | Bool
  | Unit
  | Fun of t * t
  | Prod of t * t
  | Sum of t * t

  and t = pre_t span
end

module Bin = struct
  type pre_t =
  | Add
  | Sub
  | Mul
  | Div
  | LAnd
  | LOr
  | Lt
  | Le
  | Ge
  | Gt
  | Eq
  | Ne

  type t = pre_t span
end

module Uno = struct
  type pre_t =
  | Neg
  | Not
  | Print

  type t = pre_t span
end

module Exp = struct
  type pre_t =
  | Int of Int.t
  | String of String.t
  | True
  | False
  | Unit
  | Var of Var.t
  | Let of Var.t * t * t
  | Abs of Var.t * Type.t * t
  | App of t * t
  | Seq of t * t
  | If of t * t * t
  | Bin of Bin.t * t * t
  | Uno of Uno.t * t
  | Prod of t * t
  | Pil of t
  | Pir of t
  | Inl of Type.t * t
  | Inr of Type.t * t
  | Case of t * t * t

  and t = pre_t span
end
