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

%start <Ruby.value option> prog
%%

prog:
  | v = value { Some v }
  | EOF       { None   } ;

value:
  | LBRACE obj = obj_fields RBRACE { `Hash obj        }
  | LBRACK vl = list_fields RBRACK { `List vl         }
  | s = STRING                     { `String s        }
  | i = INT                        { `Int i           }
  | x = FLOAT                      { `Float x         }
  | TRUE                           { `Bool true       }
  | FALSE                          { `Bool false      }
  | id = ID EOL                    { `Id (id, `Any)   }
  | id = ID EQ v = value EOL       { `Id (id, v)      }
  | c = CONST EQ v = value EOL     { `Const (c, v)    }
  | c = CONST                      { `Const (c, `Any) }
  | NIL                            { `Nil            } ;

obj_fields:
    obj = separated_list(COMMA, obj_field)    { obj } ;

obj_field:
    k = value COLON v = value                 { (k, v) } ;

list_fields:
    vl = separated_list(COMMA, value)         { vl } ;
