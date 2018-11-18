module Simple = Types

module Span = struct
  open Span
  let format_t fmt s =
    Format.fprintf fmt "%i:%i to %i:%i"
      s.l.pos_lnum
      s.l.pos_cnum
      s.r.pos_lnum
      s.r.pos_cnum
end

module Int = struct
  let format_t fmt e =
    Format.fprintf fmt "%i" (fst e)
end

module Var = struct
  let format_t fmt e =
    Format.fprintf fmt "%s" (fst e)
end

module Bin = struct
  open Simple.Bin
  let format_t fmt e =
    let op = match fst e with
    | Add  -> "+"
    | Sub  -> "-"
    | Mul  -> "*"
    | Div  -> "/"
    | LAnd -> "/\\"
    | LOr  -> "\\/"
    | Lt   -> "<"
    | Le   -> "<="
    | Ge   -> ">="
    | Gt   -> ">"
    | Eq   -> "="
    | Ne   -> "!="
    in Format.fprintf fmt "%s" op
end

module Uno = struct
  open Simple.Uno
  let format_t fmt e =
    let op = match fst e with
    | Neg -> "-"
    | Not -> "not "
    in Format.fprintf fmt "%s" op
end

module Type = struct
  open Simple.Type
  let rec format_t fmt e =
    match fst e with
    | Int -> Format.fprintf fmt "int"
    | Unit -> Format.fprintf fmt "unit"
    | Bool -> Format.fprintf fmt "bool"
    | Fun (l, r) ->
      Format.fprintf fmt "%a@ ->@ %a"
        format_t l
        format_t r
    | Prod (l, r) ->
      Format.fprintf fmt "%a@ *@ %a"
        format_t l
        format_t r
    | Sum (l, r) ->
      Format.fprintf fmt "%a@ +@ %a"
        format_t l
        format_t r
end

module Exp = struct
  open Simple.Exp
  let rec format_t fmt e =
    match fst e with
    | Int n -> Int.format_t fmt n
    | True  -> Format.fprintf fmt "true"
    | False -> Format.fprintf fmt "false"
    | Unit  -> Format.fprintf fmt "()"
    | Var v -> Var.format_t fmt v
    | Let (v, e, e') ->
      Format.fprintf fmt "@[<2>let@ %a@ =@ %a@ in@ %a@]"
        Var.format_t v    
        format_t e
        format_t e'
    | Abs (v, t, e) ->
      Format.fprintf fmt "@[<2>λ%a@ :@ %a.@ %a@]"
        Var.format_t v
        Type.format_t t 
        format_t e
    | App (e, e') ->
      Format.fprintf fmt "@[<2>%a@ %a@]"
        format_t e
        format_t e'
    | If (b, t, f) ->
      Format.fprintf fmt "@[<2>if@ %a@ then@ %a@ else@ %a@]"
        format_t b
        format_t t
        format_t f
    | Bin (op, l, r) ->
      Format.fprintf fmt "@[<2>%a@ %a@ %a@]"
        format_t l
        Bin.format_t op
        format_t r
    | Uno (op, e) ->
      Format.fprintf fmt "@[<2>%a%a@]"
        Uno.format_t op
        format_t e
    | Prod (l, r) ->
      Format.fprintf fmt "@[<2>(%a,@ %a)@]"
        format_t l
        format_t r
    | Pil e ->
      Format.fprintf fmt "%a.0"
        format_t e
    | Pir e ->
      Format.fprintf fmt "%a.1"
        format_t e
    | Inl (t, e) ->
      Format.fprintf fmt "@[<2>inl[%a]@ %a@]"
        Type.format_t t
        format_t e
    | Inr (t, e) ->
      Format.fprintf fmt "@[<2>inr[%a]@ %a@]"
        Type.format_t t
        format_t e
    | Case (e, l, r) ->
      Format.fprintf fmt "@[<2>case@ %a@ of@ %a@ |@ %a@]"
        format_t e
        format_t l
        format_t r
end

module Typed = struct
  open Typed
  let rec format_t fmt e =
    match e with
    | Int -> Format.fprintf fmt "int"
    | Unit -> Format.fprintf fmt "unit"
    | Bool -> Format.fprintf fmt "bool"
    | Fun (l, r) ->
      Format.fprintf fmt "%a@ ->@ %a"
        format_t l
        format_t r
    | Prod (l, r) ->
      Format.fprintf fmt "%a@ *@ %a"
        format_t l
        format_t r
    | Sum (l, r) ->
      Format.fprintf fmt "%a@ +@ %a"
        format_t l
        format_t r
    
  let format_result fmt r =
    let open Check in
    match r with
    | Ok t -> format_t fmt t
    | Error (e, span) -> match e with
    | Expected (t, t') ->
      Format.fprintf fmt
        "%a: expected %a but found %a"
        Span.format_t span
        format_t t
        format_t t'
    | Unbound v ->
      Format.fprintf fmt
        "%a: unbound variable %a"
        Span.format_t span
        Var.format_t v
    | NotFunction t ->
      Format.fprintf fmt
        "%a: expected function but found %a"
        Span.format_t span
        format_t t
    | NotProd t ->
      Format.fprintf fmt
        "%a: expected product but found %a"
        Span.format_t span
        format_t t
    | NotSum t ->
      Format.fprintf fmt
        "%a: expected function but found %a"
        Span.format_t span
        format_t t
end

module Value = struct
  open Eval.Value
  let rec format_t fmt v =
    match v with
    | Int n -> Format.fprintf fmt "%i" n
    | Bool true -> Format.fprintf fmt "true"
    | Bool false -> Format.fprintf fmt "true"
    | Unit -> Format.fprintf fmt "()"
    | Prod (e, e') ->
      Format.fprintf fmt "(%a, %a)"
        format_t e
        format_t e'
    | Left (t, e) ->
      Format.fprintf fmt "inl[%a] %a"
        Typed.format_t t
        format_t e
    | Right (t, e) ->
      Format.fprintf fmt "inr[%a] %a"
        Typed.format_t t
        format_t e
    | Fun (v, _, e) ->
      Format.fprintf fmt "λ%a. %a"
        Var.format_t v
        Exp.format_t e
end
