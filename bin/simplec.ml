open Simple
open Core

exception ParseError of Span.t
exception TypeError of (Check.cause * Span.t)

type mode =
| Parse
| Type

let mode = Command.Arg_type.create
  begin function
  | "parse" | "p" -> Parse
  | "type"  | "t" -> Type
  | mode -> (eprintf "Invalid mode %s" mode); exit 1
  end

type target =
| Dir of string
| File of string

let target = Command.Arg_type.create
  begin fun file ->
    if Sys.is_file file = `Yes then File file else
    if Sys.is_directory file = `Yes then Dir file else
    (eprintf "Could not find %s." file; exit 1)
  end

let parse file =
  let lexbuf = file
  |> In_channel.create ~binary:false
  |> Lexing.from_channel
  in try lexbuf
  |> Parse.program Lex.token
  with
  | Parse.Error -> raise (ParseError (Lex.span lexbuf))

let print_parsed program = program
  |> Print.Exp.format_t Format.std_formatter
  |> Format.print_newline

let check program =
  match Check.check_exp program Check.Env.empty with
  | Ok t -> (t, program)
  | Error (cause, span) -> raise (TypeError (cause, span))

let print_typed program = program
  |> Print.Typed.format_t Format.std_formatter
  |> Format.print_newline

let eval program =
  Eval.eval_exp program Value.Environment.empty

let print_value value = value
  |> Print.Value.format_t Format.std_formatter
  |> Format.print_newline

let process dir f =
  Sys.chdir dir;
  let f () file =
    if Sys.is_directory file = `Yes then () else
    let length = String.length file in
    let div = String.make length '-' in
    Out_channel.printf "\n%s\n%s\n%s\n\n" div file div;
    f file;
    ()
  in
  let _ = Sys.fold_dir ~init:() ~f (Sys.getcwd ()) in ()

let driver mode target =
  let go file = try match mode with
  | Some Parse -> file |> parse |> print_parsed
  | Some Type -> file |> parse |> check |> fst |> print_typed
  | None -> file |> parse |> check |> snd |> eval |> print_value
  with
  | Lex.LexError (_, span) ->
    Format.fprintf Format.std_formatter
      "%a: unclosed string literal"
      Print.Span.format_t span;
    Format.print_newline ();
  | ParseError span ->
    Print.Span.format_t Format.std_formatter span;
    Format.print_newline ()
  | TypeError (cause, span) ->
    Print.Typed.format_error Format.std_formatter (cause, span);
    Format.print_newline ()
  in
  match target with
  | File file -> go file
  | Dir dir -> process dir go

let () =
  let open Command.Let_syntax in
  Command.basic
    ~summary:"Compiler front-end for the simply-typed lambda calculus"
    [%map_open
      let mode = flag "mode" (optional mode) ~doc:"Stop at an earlier compilation phase"
      and target = anon ("FILE" %: target) in
      fun () -> driver mode target
    ]
  |> Command.run
