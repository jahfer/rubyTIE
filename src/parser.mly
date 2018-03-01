%token <int> INT
%token <float> FLOAT
%token <string> STRING
%token TRUE
%token FALSE
%token NIL
%token LBRACE
%token RBRACE
%token LBRACK
%token RBRACK
%token LPAREN
%token RPAREN
%token COLON
%token COMMA
%token EOF
%token <string> ID
%token <string> CONST
%token EQ
%token DEF
%token END
%token EOS

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
  s = statement statement_end { s }
  ;

statement_end: EOS | EOF {};

statement:
  | ref = ref                       { ref }
  | id = ID EQ v = value            { id, v, Ruby.typeof v }
  | c = CONST                       { c, None, TConst (Ruby.Type_variable.gen_next ()) }
  | c = CONST EQ v = value          { c, v, TConst (Ruby.typeof v) }
  | DEF fn = ID p = params EOS? END { fn, Func p, TFunc (Ast.arg_types p, Ruby.Type_variable.gen_next ()) }
  | v = value                       { "(orphan)", v, Ruby.typeof v }
  ;

ref:
  id = ID { id, None, (Ruby.Type_variable.gen_next ()) } ;

value:
  | LBRACE obj = obj_fields RBRACE { Hash obj }
  | LBRACK vl = list_fields RBRACK { Array vl }
  | COLON s = ID                   { Symbol s }
  | s = STRING                     { String s }
  | i = INT                        { Int i }
  | x = FLOAT                      { Float x }
  | TRUE                           { Bool true }
  | FALSE                          { Bool false }
  | NIL                            { Nil }
  ;

params:
  LPAREN p = separated_list(COMMA, ref) RPAREN { p } ;

obj_fields:
  obj = separated_list(COMMA, obj_field)    { obj } ;

obj_field:
  k = value COLON v = value                 { k, v } ;

list_fields:
  vl = separated_list(COMMA, value)         { vl } ;
