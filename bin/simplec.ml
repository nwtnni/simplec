open Simple
open Core

exception ParseError of string

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
  |> Print.Exp.format_t Format.std_formatter
  |> Format.print_newline
  with
  | Parse.Error -> raise (ParseError (Span.to_string (Lex.span lexbuf)))

let driver early target =
  match target with
  | File file -> parse file
  | Dir dir ->
    Sys.chdir dir;
    let go halt file =
      if file = "." || file = ".." || halt then false
      else try parse file; false with
      | ParseError span -> eprintf "%s" span; early
    in
    let _ = Sys.fold_dir ~init:false ~f:go (Sys.getcwd ()) in ()

let () =
  let open Command.Let_syntax in
  Command.basic
    ~summary:"Compiler front-end for the simply-typed lambda calculus"
    [%map_open
      let early = flag "early" no_arg ~doc:"halt after first error"
      and target = anon ("FILE" %: target) in
      fun () -> driver early target
    ]
  |> Command.run
