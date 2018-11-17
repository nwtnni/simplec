type t = {
  l: Lexing.position;
  r: Lexing.position;
}

let merge s1 s2 = { l = s1.l; r = s2.r }

let to_string s =
  Format.sprintf "%i:%i to %i:%i"
    s.l.pos_lnum 
    s.l.pos_cnum
    s.r.pos_lnum
    s.r.pos_cnum
