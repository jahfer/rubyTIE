open Core
open Typed_ruby.Lexer
open Lexing

let print_position outx lexbuf =
  let pos = lexbuf.lex_curr_p in
  fprintf outx "%s:%d:%d" pos.pos_fname
    pos.pos_lnum (pos.pos_cnum - pos.pos_bol + 1)

let state : Typed_ruby.Lexer.lex_state = {
  pending_termination = false;
  at_eos = false;
  paren_level = 0;
  lambda_stack = [];
  fn_call = false;
}

let parse_with_error lexbuf =
  try Typed_ruby.Parser.prog (Typed_ruby.Lexer.read state) lexbuf with
  | SyntaxError msg ->
    fprintf stderr "%a: %s\n" print_position lexbuf msg;
    None
  | Typed_ruby.Parser.Error ->
    let tok = Lexing.lexeme lexbuf in
    fprintf stderr "%a: syntax error ('%s')\n" print_position lexbuf tok;
    exit (-1)

let rec parse_and_print lexbuf =
  match parse_with_error lexbuf with
  | Some (expr) ->
    printf "%a\n" Typed_ruby.Ruby.Printer.print_expr_ast expr;
    Typed_ruby.Ruby.Type_variable.reset ();
    parse_and_print lexbuf
  | None -> ()

(* let () =
  Command.basic ~summary:"Parse and display Ruby"
    Command.Param.(empty +> anon ("filename" %: file))
    loop
  |> Command.run *)

open Cmdliner

let rbt filename =
  let inx = In_channel.create filename in
  let lexbuf = Lexing.from_channel inx in
  lexbuf.lex_curr_p <- { lexbuf.lex_curr_p with pos_fname = filename };
  parse_and_print lexbuf;
  In_channel.close inx;
  `Ok true

let srcs =
  let doc = "Source file(s) to copy." in
  Arg.(required & ((pos 0 (some string) None)) & (info [] ~docv:"DEST" ~doc))

let cmd =
  let doc = "Parse and display Ruby" in
  let exits = Term.default_exits in
  Term.(ret (const rbt $ srcs)),
  Term.info "rbt" ~version:"v1.0.2" ~exits ~doc

let () =
  Term.(exit @@ eval cmd)

