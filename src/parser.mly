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
  | EOF         { None   }
  ;

id:
  | DEF fn = ID p = params END     { gen_id_name, Func p, TFunc (TAny) }
  | id = ID                        { id, None, TAny }
  | id = ID EQ v = value           { id, v, rb_typeof v }
  | c = CONST EQ v = value         { c, v, TConst (rb_typeof v) }
  | c = CONST                      { c, None, TConst (TAny) }
  | v = value                      { gen_id_name, v, rb_typeof v }
  ;

params:
  | LPAREN vl = list_fields RPAREN { vl }
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

obj_fields:
    obj = separated_list(COMMA, obj_field)    { obj } ;

obj_field:
    k = value COLON v = value                 { k, v } ;

list_fields:
    vl = separated_list(COMMA, value)         { vl } ;
