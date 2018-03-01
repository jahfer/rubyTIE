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
  open Ruby
%}

%start <Ruby.id option> prog

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
  | id = ID EQ v = value            { id, v, rb_typeof v }
  | c = CONST                       { c, None, TConst (gen_polymorphic_type ()) }
  | c = CONST EQ v = value          { c, v, TConst (rb_typeof v) }
  | DEF fn = ID p = params EOS? END { fn, Func p, TFunc (arg_types p, gen_polymorphic_type ()) }
  | v = value                       { "(orphan)", v, rb_typeof v }
  ;

ref:
  id = ID { id, None, (gen_polymorphic_type ()) } ;

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
