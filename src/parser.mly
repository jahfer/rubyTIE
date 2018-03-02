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

%start <Ast.expr option> prog

%%

prog:
  | s = top_statement { Some s }
  | EOF               { None   }
  ;

top_statement:
  s = statement top_statement_end { s } ;

statement_end:
  EOS { };

top_statement_end:
  statement_end | EOF { };

statement:
  | ref = identifier             { Value(ref) }
  | id = ID EQ v = rhs_assign    { Value(id, v, Ruby.typeof v) }
  | c = CONST                    { Value(c, None, TConst (Ruby.Type_variable.gen_new_t ())) }
  | c = CONST EQ v = rhs_assign  { Value(c, v, TConst (Ruby.typeof v)) }
  | p = primitive                { Orphan(p, Ruby.typeof p) }
  | e = expr                     { e }
  ;

rhs_assign:
  | s = statement { Ast.expr_return_value s }
  ;

expr:
  | c = command_call { c }
  | f = func     { f }
  ;

command_call:
  c = command { c } ;

command:
  | c = method_call args = command_args                          { Call(None, c, args) }
  | c1 = identifier call_op c2 = method_call                     { Call(Some(Value(c1)), c2, []) }
  | c1 = identifier call_op c2 = method_call args = command_args { Call(Some(Value(c1)), c2, args) }
  ;

call_op: DOT { } ;

method_call:
  id = FID { id } ;

command_args:
  | node = command_call { [node] }
  | args = fn_args { List.map (fun x -> Value(x)) args }
  ;

identifier:
  id = ID { id, Any, Ruby.Type_variable.gen_new_t () } ;

func:
  | DEF fn = ID args = fn_args EOS? END {
    Func(fn, args, Orphan(Any, Ruby.Type_variable.gen_new_t ()))
  }
  | DEF fn = ID args = fn_args EOS? s = statement statement_end? END {
    Func(fn, args, s)
  }
  ;

primitive:
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
  | args = fn_args body = lambda_body   { Lambda (args, body) }
  ;

lambda_body:
  | LAMBEG s = statement statement_end? RBRACE {
    s
  }
  | LAMBEG RBRACE { Orphan(Nil, TNil) }
  ;

fn_args:
  LPAREN p = separated_list(COMMA, identifier) RPAREN { p } ;

obj_fields:
  obj = separated_list(COMMA, obj_field)    { obj } ;

obj_field:
  k = primitive COLON v = primitive             { k, v } ;

list_fields:
  vl = separated_list(COMMA, primitive)       { vl } ;
