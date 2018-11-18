# simplec

Lexer, parser, type-checker, and interpreter for the [simply typed lambda calculus][1]
with the following extensions:

- Let expressions

```
let x in 5 in x
```

- Integer, boolean, string, and unit types

```
let x = 1 in
let b = true in
let s = "string" in
let u = () in
()
```

- Various binary and unary operators

```
let lt = 1 < 2 in
let le = 1 <= 2 in
let ge = 1 >= 2 in
let gt = 1 > 2 in
let eq = 1 = 2 in
let ne = 1 != 2 in
let and = true /\ false in
let or = true \/ false in
let not = not true in
let neg = -5 in
()
```

- String length and concatenation operators

```
let s = "test" in
let l = length s in
let s = s ^ s in
s
```

- Product types

```
let p = (1, 2) in
(p.0, p.1)
```

- Sum types

```
let l = inl[int + string] 5 in
let r = inr[int + string] "hello" in
case l of
| λx: int. x
| λs: string. length s
```

- Printing and sequencing operators

```
print 1;
print 2
```

[1]: https://en.wikipedia.org/wiki/Simply_typed_lambda_calculus
