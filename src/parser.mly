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
  s = statement top_statement_end { s } ;

internal_statement:
  s = statement statement_end { s } ;

statement_end:
  EOS { };

top_statement_end:
  statement_end | EOF { };

statement:
  | ref = operation              { ref }
  | id = ID EQ v = rhs_assign    { id, v, Ruby.typeof v }
  | c = CONST                    { c, None, TConst (Ruby.Type_variable.gen_next ()) }
  | c = CONST EQ v = rhs_assign  { c, v, TConst (Ruby.typeof v) }
  | f = func                     { f }
  | v = literal                  { "(orphan)", v, Ruby.typeof v }
  | e = expr                     { e }
  ;

rhs_assign:
  | l = literal { l }
  | s = statement { Ast.id_value s }

expr:
  | c = command_call { c }
  ;

command_call:
  c = command { "(call)", c, Ruby.Type_variable.gen_next () } ;

command:
  | c = method_call args = command_args                     { Call(None, c, args) }
  | c1 = fcall call_op c2 = method_call                     { Call(Some(c1), c2, []) }
  | c1 = fcall call_op c2 = method_call args = command_args { Call(Some(c1), c2, args) }
  ;

call_op: DOT { } ;

fcall:
  | id = operation   { id }
  | id = method_call { id, None, (Ruby.Type_variable.gen_next ()) }
  ;

method_call:
  id = FID { id } ;

command_args:
  | node = command_call { [node] }
  | args = args         { args }
  ;

operation:
  id = ID { id, None, (Ruby.Type_variable.gen_next ()) } ;

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
    List.rev (s2 :: List.rev s1)
  }
  | LAMBEG s = statement statement_end? RBRACE {
    [s]
  }
  | LAMBEG RBRACE { [] }
  ;

args:
  LPAREN p = separated_list(COMMA, operation) RPAREN { p } ;

obj_fields:
  obj = separated_list(COMMA, obj_field)    { obj } ;

obj_field:
  k = literal COLON v = literal             { k, v } ;

list_fields:
  vl = separated_list(COMMA, literal)       { vl } ;
