open Core
open Lexing
open Lexer

let init_state () : lex_state = {
  pending_termination = false;
  at_eos = false;
  paren_level = 0;
  lambda_stack = [];
  fn_call = false;
}

let state = init_state ()

let print_position outx lexbuf =
  let pos = lexbuf.lex_curr_p in
  fprintf outx "%s:%d:%d" pos.pos_fname
    pos.pos_lnum (pos.pos_cnum - pos.pos_bol + 1)

let parse_with_error lexbuf =
  try Parser.prog (Lexer.read state) lexbuf with
  | Lexer.SyntaxError msg ->
    fprintf stderr "%a: %s\n" print_position lexbuf msg;
    None
  | Parser.Error ->
    let tok = Lexing.lexeme lexbuf in
    fprintf stderr "%a: syntax error ('%s')\n" print_position lexbuf tok;
    exit (-1)

let print_type_error a b =
  printf "-- TYPE ERROR %s\n\n" (String.make 40 '-');
  printf "Type `%s` is not compatible with type `%s`\n"
    (Printer.type_to_str (Disjoint_set.find a).elem)
    (Printer.type_to_str (Disjoint_set.find b).elem);
  let () = match b.metadata with
    | Some (loc) -> 
      Location.print_loc loc;
      printf " type is initialized as `%s` here\n" (Printer.type_to_str (Disjoint_set.find b).elem);
    | None -> ()
  in
  match a.metadata with
  | Some (loc) ->
    Location.print_loc loc;
    printf " but then used as `%s` here\n" (Printer.type_to_str (Disjoint_set.find a).elem)
  | None -> ()

let rec parse_and_print lexbuf =
  match parse_with_error lexbuf with
  | Some (expr) -> let open Typed_ast in
    let typed_ast = try (to_typed_ast expr) with
      | Constraint_engine.TypeError (a, b) ->
        print_type_error a b;
        exit (-1)
    in printf "%a\n" (ExpressionPrinter.print_expression ~indent:1) typed_ast;
    parse_and_print lexbuf
  | None -> ()

let parse_from_filename filename =
  let inx = In_channel.create filename in
  let lexbuf = Lexing.from_channel inx in
  lexbuf.lex_curr_p <- { lexbuf.lex_curr_p with pos_fname = filename };
  parse_and_print lexbuf;
  In_channel.close inx;
