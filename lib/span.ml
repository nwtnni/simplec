type position = Lexing.position

let sexp_of_position (pos: position) : Sexplib.Sexp.t =
  let open Sexplib.Sexp in
  List [
    Atom pos.pos_fname;
    Atom (string_of_int pos.pos_lnum); 
    Atom (string_of_int pos.pos_bol);
    Atom (string_of_int pos.pos_cnum);
  ]

let position_of_sexp (s: Sexplib.Sexp.t) : position =
  let open Sexplib.Sexp in
  match s with 
  | List [Atom f; Atom l; Atom b; Atom c] ->
      { pos_fname = f;
        pos_lnum = int_of_string l;
        pos_bol = int_of_string b;
        pos_cnum = int_of_string c; }
  | _ -> failwith "[INTERNAL ERROR]: not a position"

type t = {
  l: position;
  r: position;
} [@@deriving sexp]

let merge s1 s2 = { l = s1.l; r = s2.r }

let to_string s =
  Format.sprintf "%i:%i to %i:%i"
    s.l.pos_lnum 
    s.l.pos_cnum
    s.r.pos_lnum
    s.r.pos_cnum
