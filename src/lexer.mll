{
  open Lexing
  open Parser
  open Printf

  exception SyntaxError of string

  type lex_state = {
    (* newline-agnostic unterminated statement *)
    mutable pending_termination : bool;
    mutable at_eos : bool;
    mutable paren_level : int;
    mutable lambda_stack : int list;
  }

  let next_line lexbuf =
    let pos = lexbuf.lex_curr_p in
    lexbuf.lex_curr_p <-
      { pos with pos_bol = lexbuf.lex_curr_pos;
                pos_lnum = pos.pos_lnum + 1
      }

  let newline_agnostic_tok state =
    state.pending_termination <- true;
    state.at_eos <- false
  and terminating_tok state =
    state.pending_termination <- false;
    state.at_eos <- false
  and ack_tok state =
    state.at_eos <- false

  let print_state state = printf {str|{
  pending_termination: %B,
  at_eos: %B
} |str} state.pending_termination state.at_eos;
    printf "\n"
}

let int = '-'? ['0'-'9'] ['0'-'9']*
let digit = ['0'-'9']
let frac = '.' digit*
let exp = ['e' 'E'] ['-' '+']? digit+
let float = digit* frac? exp?
let white = [' ' '\t']+
let newline = '\r' | '\n' | "\r\n"
let id = ['@']* ['a'-'z'] ['a'-'z' 'A'-'Z' '0'-'9' '_']*
let const = ['A'-'Z'] ['a'-'z' 'A'-'Z' '_']*

rule read state = parse
  | white    { read state lexbuf }
  | newline  {
      if state.pending_termination || state.at_eos then begin
        next_line lexbuf; read state lexbuf
      end else begin
        next_line lexbuf; state.at_eos <- true; EOS
      end
  }
  | "def"    { newline_agnostic_tok state; DEF }
  | "->"     {
      state.lambda_stack <- state.paren_level :: state.lambda_stack;
      LAMBDA
  }
  | '"'      { ack_tok state; read_string (Buffer.create 17) lexbuf }
  | ':'      { ack_tok state; COLON }
  | ';'      { ack_tok state; EOS }
  | ','      { ack_tok state; COMMA }
  | '='      { ack_tok state; EQ }
  | '+'      { ack_tok state; PLUS }
  | '{'      { newline_agnostic_tok state;
    match state.lambda_stack with
    | el :: tl when el = state.paren_level ->
      state.lambda_stack <- tl; LAMBEG
    | _ -> LBRACE
  }
  | '['      { newline_agnostic_tok state; LBRACK }
  | '('      { newline_agnostic_tok state; LPAREN }
  | '}'      { terminating_tok state;      RBRACE }
  | ']'      { terminating_tok state;      RBRACK }
  | ')'      { terminating_tok state;      RPAREN }
  | int      { ack_tok state; INT (int_of_string (Lexing.lexeme lexbuf)) }
  | float    { ack_tok state; FLOAT (float_of_string (Lexing.lexeme lexbuf)) }
  | "true"   { ack_tok state; TRUE }
  | "false"  { ack_tok state; FALSE }
  | "nil"    { ack_tok state; NIL }
  | "end"    { ack_tok state; END }
  | const    { ack_tok state; CONST (Lexing.lexeme lexbuf) }
  | id       { terminating_tok state; ID (Lexing.lexeme lexbuf) }
  | _        { raise (SyntaxError (sprintf "Unexpected char: '%s'" (Lexing.lexeme lexbuf))) }
  | eof      { EOF }

and read_string buf = parse
  | '"' { STRING (Buffer.contents buf) }
  | '\\' '/'  { Buffer.add_char buf '/'; read_string buf lexbuf }
  | '\\' '\\' { Buffer.add_char buf '\\'; read_string buf lexbuf }
  | '\\' 'b'  { Buffer.add_char buf '\b'; read_string buf lexbuf }
  | '\\' 'f'  { Buffer.add_char buf '\012'; read_string buf lexbuf }
  | '\\' 'n'  { Buffer.add_char buf '\n'; read_string buf lexbuf }
  | '\\' 'r'  { Buffer.add_char buf '\r'; read_string buf lexbuf }
  | '\\' 't'  { Buffer.add_char buf '\t'; read_string buf lexbuf }
  | [^ '"' '\'' '\\']+
    { Buffer.add_string buf (Lexing.lexeme lexbuf);
      read_string buf lexbuf
    }
  | _ { raise (SyntaxError ("Illegal string character: " ^ Lexing.lexeme lexbuf)) }
  | eof { raise (SyntaxError ("String is not terminated")) }
