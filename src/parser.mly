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

%start <Ruby.value option> prog
%%

prog:
  | v = value EOL { Some v }
  | EOF           { None   } ;

params:
  | LPAREN vl = list_fields RPAREN { vl } ;

value:
  | LBRACE obj = obj_fields RBRACE { `Hash obj        }
  | LBRACK vl = list_fields RBRACK { `Array vl        }
  | s = STRING                     { `String s        }
  | i = INT                        { `Int i           }
  | x = FLOAT                      { `Float x         }
  | TRUE                           { `Bool true       }
  | FALSE                          { `Bool false      }
  | DEF fn = ID p = params END     { `Func ((fn, `TAny), p) }
  | id = ID                        { `Id ((id, `TAny), `None)        }
  | id = ID EQ v = value           { `Id ((id, `TAny), v) }
  | c = CONST EQ v = value         { `Const ((c, `TAny), v) }
  | c = CONST                      { `Const ((c, `TAny), `None) }
  | NIL                            { `Nil            } ;

obj_fields:
    obj = separated_list(COMMA, obj_field)    { obj } ;

obj_field:
    k = value COLON v = value                 { (k, v) } ;

list_fields:
    vl = separated_list(COMMA, value)         { vl } ;
