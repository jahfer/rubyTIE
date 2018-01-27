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
%token EOL
%token DEF
%token END

%{
  open Ruby
%}

%start <Ruby.id option> prog
%%

prog:
  | id = id EOL { Some id }
  | EOF         { None    }
  ;

ref_value:
  id = ID { id, None, (gen_polymorphic_type ()) } ;

id:
  | ref = ref_value                { ref }
  | id = ID EQ v = value           { id, v, rb_typeof v }
  | c = CONST                      { c, None, TConst (gen_polymorphic_type ()) }
  | c = CONST EQ v = value         { c, v, TConst (rb_typeof v) }
  | DEF fn = ID p = params END     { fn, Func p, TFunc (arg_types p, gen_polymorphic_type ()) }
  | v = value                      { "(orphan)", v, rb_typeof v }
  ;

value:
  | LBRACE obj = obj_fields RBRACE { Hash obj }
  | LBRACK vl = list_fields RBRACK { Array vl }
  | s = STRING                     { String s }
  | i = INT                        { Int i }
  | x = FLOAT                      { Float x }
  | TRUE                           { Bool true }
  | FALSE                          { Bool false }
  | NIL                            { Nil }
  ;

params:
  LPAREN p = separated_list(COMMA, ref_value) RPAREN { p } ;

obj_fields:
  obj = separated_list(COMMA, obj_field)    { obj } ;

obj_field:
  k = value COLON v = value                 { k, v } ;

list_fields:
  vl = separated_list(COMMA, value)         { vl } ;
