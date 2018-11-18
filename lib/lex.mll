{
  open Core
  open Parse

  type cause =
  | UnclosedString

  exception LexError of (cause * Span.t)

  let span lexbuf =
    let open Span in
    { l = Lexing.lexeme_start_p lexbuf;
      r = Lexing.lexeme_end_p lexbuf; }
}

let var = ['a'-'z'] ['a'-'z' 'A'-'Z' '0'-'9' ''' '_']*
let int = ['0'-'9'] ['0'-'9']*
let ws = [' ' '\t' '\r']
 
rule token = parse
| '\n'           { Lexing.new_line lexbuf; token lexbuf }
| ws             { token lexbuf }
| int as n       { INT((int_of_string n, span lexbuf)) }
| "int"          { INT_TYPE(span lexbuf) }
| "string"       { STRING_TYPE(span lexbuf) }
| "bool"         { BOOL_TYPE(span lexbuf) }
| "unit"         { UNIT_TYPE(span lexbuf) }
| "->"           { TO(span lexbuf) }
| "()"           { UNIT(span lexbuf) }
| "true"         { TRUE(span lexbuf) }
| "false"        { FALSE(span lexbuf) }
| "<"            { LT(span lexbuf) }
| "<="           { LE(span lexbuf) }
| ">="           { GE(span lexbuf) }
| ">"            { GT(span lexbuf) }
| "="            { EQ(span lexbuf) }
| "!="           { NE(span lexbuf) }
| "+"            { ADD(span lexbuf) }
| "-"            { SUB(span lexbuf) }
| "*"            { MUL(span lexbuf) }
| "/"            { DIV(span lexbuf) }
| "^"            { CAT(span lexbuf) }
| "/\\"          { LAND(span lexbuf) }
| "\\/"          { LOR(span lexbuf) }
| "λ" | "lambda" { LAMBDA(span lexbuf) }
| "."            { DOT(span lexbuf) }
| ","            { COMMA(span lexbuf) }
| ":"            { COLON(span lexbuf) }
| ";"            { SEMICOLON(span lexbuf) }
| "not"          { NOT(span lexbuf) }
| "("            { LPAREN(span lexbuf) }
| ")"            { RPAREN(span lexbuf) }
| ".0"           { PIL(span lexbuf) }
| ".1"           { PIR(span lexbuf) }
| "["            { LBRACE(span lexbuf) }
| "]"            { RBRACE(span lexbuf) }
| "inl"          { INL(span lexbuf) }
| "inr"          { INR(span lexbuf) }
| "case"         { CASE(span lexbuf) }
| "of"           { OF(span lexbuf) }
| "|"            { OR(span lexbuf) }
| "if"           { IF(span lexbuf) }
| "then"         { THEN(span lexbuf) }
| "else"         { ELSE(span lexbuf) }
| "let"          { LET(span lexbuf) }
| "in"           { IN(span lexbuf) }
| "print"        { PRINT(span lexbuf) }
| "length"       { LENGTH(span lexbuf) }
| '"'            { str (Buffer.create 10) lexbuf }
| var as v       { VAR((v, span lexbuf)) }
| eof            { EOF(span lexbuf) }

and str buf = parse
| '"'       { STRING(Buffer.contents buf, span lexbuf) }
| '\\' 't'  { Buffer.add_char buf '\t'; str buf lexbuf }
| '\\' 'n'  { Buffer.add_char buf '\n'; str buf lexbuf }
| '\\' '\\' { Buffer.add_char buf '\\'; str buf lexbuf }
| '\\' '"'  { Buffer.add_char buf '"'; str buf lexbuf }
| eof       { raise (LexError (UnclosedString, span lexbuf)) }
| _ as c    { Buffer.add_char buf c; str buf lexbuf }
