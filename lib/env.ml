open Core
open Types

module Make (Binding : sig
  type t
  type error
  val unbound: Var.t -> error
end) = struct

  type t = (Var.t * Binding.t) list

  let empty = []

  let insert env v t =
    (v, t) :: env

  let find (env: t) (v: Var.t) : (Binding.t, Binding.error * Span.t) result =
    let f (v', _) = (fst v = fst v') in
    match List.find ~f env with
    | None -> Error (Binding.unbound v, snd v)
    | Some (_, t) -> Ok t
end
