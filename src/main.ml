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
  Format.fprintf outx "%s:%d:%d" pos.pos_fname
    pos.pos_lnum (pos.pos_cnum - pos.pos_bol + 1)

let parse_with_error lexbuf =
  try Parser.prog (Lexer.read state) lexbuf with
  | Lexer.SyntaxError msg ->
    Format.fprintf Format.std_formatter "%a: %s\n" print_position lexbuf msg;
    None
  | Parser.Error ->
    let tok = Lexing.lexeme lexbuf in
    Format.fprintf Format.std_formatter "%a: syntax error ('%s')\n" print_position lexbuf tok;
    exit (-1)

let parse_buf_to_ast lexbuf =
  (* iterate through buffer to cumulatively build untyped AST *)
  let rec build_untyped_ast lexbuf acc =
    match parse_with_error lexbuf with
    | Some (expr) ->
      build_untyped_ast lexbuf ((Typed_ast.annotate expr) :: acc)
    | None -> acc
  in let untyped_ast : 'a Ast.expression list = build_untyped_ast lexbuf [] in
  (* Find constraints based on interaction within AST *)
  let open Constraint_engine in
  let constraints = Constraints.Map.empty
    |> List.fold_right (fun x acc ->
        try build_constraints acc x with
        | Constraint_engine.TypeError (a, b) -> Printer.print_type_error a b; exit (-1)
      ) untyped_ast in

  (* Simplify constraints *)
  let constraints = constraints |> Constraint_engine.simplify_map in
  let _ = constraints |> Typed_ast.ExpressionPrinter.print_constraint_map in

  (* Solve for types *)
  let _ = try constraints |> Constraint_engine.solve with
    | Constraint_engine.TypeError (a, b) ->
      Printer.print_type_error a b; exit (-1)
  in
  untyped_ast
    |> List.map (fun ast ->
        Typed_ast.apply_types ast constraints)
    |> List.rev
    |> List.iter (fun ast ->
        Printf.printf "%a\n" (Typed_ast.ExpressionPrinter.print_expression ~indent:1) ast)

let parse_from_filename filename =
  let inx = Core.In_channel.create filename in
  let lexbuf = Lexing.from_channel inx in
  lexbuf.lex_curr_p <- { lexbuf.lex_curr_p with pos_fname = filename };
  parse_buf_to_ast lexbuf;
  Core.In_channel.close inx;
