open Simple
open Core

exception ParseError of Span.t

type mode =
| Parse
| Type
| Eval

let mode = Command.Arg_type.create
  begin function
  | "parse" | "p" -> Parse
  | "type"  | "t" -> Type
  | "eval"  | "e" -> Eval
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
  Check.check_exp program Check.Env.empty

let print_typed program = program
  |> Print.Typed.format_result Format.std_formatter
  |> Format.print_newline

let process dir early f =
  Sys.chdir dir;
  let go halt file =
    if Sys.is_directory file = `Yes || halt then halt else
    try
      let length = String.length file in
      let div = String.make length '-' in
      Out_channel.printf "\n%s\n%s\n%s\n\n" div file div;
      f file;
      halt
    with
    | ParseError span -> Print.Span.format_t Format.err_formatter span; early
  in
  let _ = Sys.fold_dir ~init:false ~f:go (Sys.getcwd ()) in ()

let driver early mode target =
  match mode, target with
  | Parse, File file -> parse file |> print_parsed
  | Type, File file -> parse file |> check |> print_typed
  | Parse, Dir dir -> process dir early (fun file -> parse file |> print_parsed)
  | Type, Dir dir -> process dir early (fun file -> parse file |> check |> print_typed)
  | _ -> failwith "unimplemented"

let () =
  let open Command.Let_syntax in
  Command.basic
    ~summary:"Compiler front-end for the simply-typed lambda calculus"
    [%map_open
      let early = flag "early" no_arg ~doc:"halt after first error"
      and mode = anon ("MODE" %: mode)
      and target = anon ("FILE" %: target) in
      fun () -> driver early mode target
    ]
  |> Command.run
