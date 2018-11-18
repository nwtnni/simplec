type t = {
  l: Lexing.position;
  r: Lexing.position;
}

let merge s1 s2 = { l = s1.l; r = s2.r }
