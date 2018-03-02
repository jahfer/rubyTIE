%token <int> INT
%token <float> FLOAT
%token <string> STRING
%token TRUE FALSE NIL
%token LBRACE RBRACE LBRACK RBRACK LPAREN RPAREN LAMBEG
%token COLON COMMA
%token EOS EOF
%token <string> ID FID
%token <string> CONST
%token EQ DEF END LAMBDA DOT

%{
  open Ast
%}

%start <Ast.id option> prog

%%

prog:
  | s = top_statement { Some s }
  | EOF               { None   }
  ;

top_statement:
  s = statement top_statement_end { Printf.printf "top_statement\n"; s } ;

internal_statement:
  s = statement statement_end { s } ;

statement_end:
  EOS { Printf.printf "statement_end\n" };

top_statement_end:
  statement_end | EOF { Printf.printf "top_statement_end\n" };

statement:
  | ref = operation              { Printf.printf "operation\n"; ref }
  | id = ID EQ v = literal       { Printf.printf "id=\n"; id, v, Ruby.typeof v }
  | c = CONST                    { Printf.printf "const\n"; c, None, TConst (Ruby.Type_variable.gen_next ()) }
  | c = CONST EQ v = literal     { Printf.printf "const=\n"; c, v, TConst (Ruby.typeof v) }
  | f = func                     { Printf.printf "func\n"; f }
  | v = literal                  { Printf.printf "literal\n"; "(orphan)", v, Ruby.typeof v }
  | e = expr                     { Printf.printf "expr\n"; e }
  ;

expr:
  | c = command_call { c }
  ;

command_call:
  c = command { "(call)", c, Ruby.Type_variable.gen_next () } ;

command:
  | c = method_call args = command_args                     { Printf.printf("command 1\n"); Call(None, c, args) }
  | c1 = fcall call_op c2 = method_call                     { Printf.printf("command 2\n"); Call(Some(c1), c2, []) }
  | c1 = fcall call_op c2 = method_call args = command_args { Printf.printf("command 2\n"); Call(Some(c1), c2, args) }
  ;

call_op: DOT { Printf.printf "call_op\n"; } ;

fcall:
  | id = operation   { Printf.printf "fcall\n"; id }
  | id = method_call { Printf.printf "fcall\n"; id, None, (Ruby.Type_variable.gen_next ()) }
  ;

method_call:
  id = FID { Printf.printf "method_call\n"; id } ;

command_args:
  | node = command_call { [node] }
  | args = args         { args }
  ;

operation:
  id = ID { Printf.printf "operation\n"; id, None, (Ruby.Type_variable.gen_next ()) } ;

func:
  DEF fn = ID p = args EOS? END {
    fn, Func p, TFunc (Ast.arg_types p, Ruby.Type_variable.gen_next ())
  }
  ;

literal:
  | LBRACE obj = obj_fields RBRACE { Hash obj }
  | LBRACK vl = list_fields RBRACK { Array vl }
  | COLON s = ID                   { Symbol s }
  | s = STRING                     { String s }
  | i = INT                        { Int i }
  | x = FLOAT                      { Float x }
  | TRUE                           { Bool true }
  | FALSE                          { Bool false }
  | NIL                            { Nil }
  | LAMBDA l = lambda              { l }
  ;

lambda:
  | body = lambda_body               { Lambda ([], body) }
  | args = args body = lambda_body   { Lambda (args, body) }
  ;

lambda_body:
  | LAMBEG s1 = internal_statement+ s2 = statement statement_end? RBRACE {
    Printf.printf "lambda_body\n"; List.rev (s2 :: List.rev s1)
  }
  | LAMBEG s = statement statement_end? RBRACE {
    Printf.printf "lambda_body\n"; [s]
  }
  | LAMBEG RBRACE { Printf.printf "lambda_body\n"; [] }
  ;

args:
  LPAREN p = separated_list(COMMA, operation) RPAREN { Printf.printf "args\n"; p } ;

obj_fields:
  obj = separated_list(COMMA, obj_field)    { obj } ;

obj_field:
  k = literal COLON v = literal             { k, v } ;

list_fields:
  vl = separated_list(COMMA, literal)       { vl } ;
