open Simple

exception ParseError of string

type mode =
| Parse
| Type

let () =
  let file = open_in (Sys.argv.(1)) in
  let lexbuf = Lexing.from_channel file in
  try
    let program = Parse.program Lex.token lexbuf in
    Print.Exp.format_t Format.std_formatter program;
    Format.print_flush ();
    print_newline ()
  with
  | Parse.Error -> raise (ParseError (Span.to_string (Lex.span lexbuf)))
