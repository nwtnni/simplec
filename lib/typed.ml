type t =
| Int
| Bool
| Unit
| Fun of t * t
| Prod of t * t
| Sum of t * t
[@@deriving eq]
