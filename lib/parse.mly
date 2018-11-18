%{
  open Types

  let node = fst
  let span = snd
%}

(* Tokens *)

%token <Types.Int.t> INT
%token <Types.Var.t> VAR
%token <Span.t> UNIT
%token <Span.t> TRUE FALSE
%token <Span.t> LT LE GE GT                           (* int -> int -> bool *)
%token <Span.t> EQ NE                                 (* ∀t. t -> t -> bool *)
%token <Span.t> ADD SUB MUL DIV                       (* int -> int -> int  *)
%token <Span.t> LAND LOR NOT                          (* bool -> bool -> bool *)
%token <Span.t> LAMBDA DOT                            (* Abstraction *)
%token <Span.t> INT_TYPE BOOL_TYPE UNIT_TYPE TO COLON (* Types *)
%token <Span.t> LPAREN RPAREN COMMA PIL PIR           (* Products *)
%token <Span.t> INL INR LBRACE RBRACE CASE OF OR      (* Sums *)
%token <Span.t> IF THEN ELSE                          (* If statements *)
%token <Span.t> LET IN                                (* Let statements *)
%token <Span.t> PRINT
%token <Span.t> SEMICOLON
%token <Span.t> EOF

(* Precedence and associativity *)

%nonassoc DOT
%nonassoc IN
%nonassoc ELSE
%nonassoc OR
%left SEMICOLON
%right PRINT

%right NOT
%left LOR
%left LAND
%left EQ NE
%left LT LE GE GT
%left ADD SUB
%left MUL DIV
%right PIL PIR

%start <Types.Exp.t> program

%%

program:
| exp EOF { $1 }

typ:
| prim_typ TO typ       { (Type.Fun($1, $3), Span.merge (span $1) (span $3)) }
| prim_typ MUL prim_typ { (Type.Prod($1, $3), Span.merge (span $1) (span $3)) }
| prim_typ ADD prim_typ { (Type.Sum($1, $3), Span.merge (span $1) (span $3)) }
| prim_typ              { $1 }

prim_typ:
| INT_TYPE          { (Type.Int, $1) }
| UNIT_TYPE         { (Type.Unit, $1) }
| BOOL_TYPE         { (Type.Bool, $1) }
| LPAREN typ RPAREN { (node $2, Span.merge $1 $3) }

exp:
| IF exp THEN exp ELSE exp     { (Exp.If($2, $4, $6), Span.merge $1 (span $6)) }
| LPAREN exp COMMA exp RPAREN  { (Exp.Prod($2, $4), Span.merge $1 $5) }
| LAMBDA VAR COLON typ DOT exp { (Exp.Abs($2, $4, $6), Span.merge $1 (span $6)) }
| exp SEMICOLON exp            { (Exp.Seq($1, $3), Span.merge (span $1) (span $3)) }
| LET VAR EQ exp IN exp        { (Exp.Let($2, $4, $6), Span.merge $1 (span $6)) }
| CASE exp OF exp OR exp       { (Exp.Case($2, $4, $6), Span.merge $1 (span $6)) }
| exp binop exp                { (Exp.Bin($2, $1, $3), Span.merge (span $1) (span $3)) }
| exp PIL                      { (Exp.Pil($1), Span.merge (span $1) $2) }
| exp PIR                      { (Exp.Pir($1), Span.merge (span $1) $2) }
| INL LBRACE typ RBRACE value  { (Exp.Inl($3, $5), Span.merge $1 (span $5)) }
| INR LBRACE typ RBRACE value  { (Exp.Inr($3, $5), Span.merge $1 (span $5)) }
| unop exp                     { (Exp.Uno($1, $2), Span.merge (span $1) (span $2)) }
| value                        { $1 }

%inline binop:
| LT   { (Bin.Lt, $1) }
| LE   { (Bin.Le, $1) } 
| GE   { (Bin.Ge, $1) } 
| GT   { (Bin.Gt, $1) } 
| EQ   { (Bin.Eq, $1) } 
| NE   { (Bin.Ne, $1) } 
| ADD  { (Bin.Add, $1) } 
| SUB  { (Bin.Sub, $1) } 
| MUL  { (Bin.Mul, $1) } 
| DIV  { (Bin.Div, $1) } 
| LAND { (Bin.LAnd, $1) } 
| LOR  { (Bin.LOr, $1) } 

%inline unop:                    
| SUB   { (Uno.Neg, $1) }
| NOT   { (Uno.Not, $1) }
| PRINT { (Uno.Print, $1) }

value:
| INT               { (Exp.Int($1), span $1) }
| VAR               { (Exp.Var($1), span $1) }
| TRUE              { (Exp.True, $1) }
| FALSE             { (Exp.False, $1) }
| UNIT              { (Exp.Unit, $1) }
| LPAREN exp RPAREN { (node $2, Span.merge $1 $3) }
