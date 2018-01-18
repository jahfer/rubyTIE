%token <int> INT
%token <float> FLOAT
%token <string> STRING
%token TRUE
%token FALSE
%token NULL
%token LEFT_BRACE
%token RIGHT_BRACE
%token LEFT_BRACK
%token RIGHT_BRACK
%token COLON
%token COMMA
%token EOF
%token <string> ID
%token <string> CONST
%token EQ

%start <Ruby.value option> prog
%%

prog:
  | v = value { Some v }
  | EOF       { None   } ;

value:
  | LEFT_BRACE; obj = obj_fields; RIGHT_BRACE { `Hash obj  }
  | LEFT_BRACK; vl = list_fields; RIGHT_BRACK { `List vl    }
  | s = STRING                                { `String s   }
  | i = INT                                   { `Int i      }
  | x = FLOAT                                 { `Float x    }
  | TRUE                                      { `Bool true  }
  | FALSE                                     { `Bool false }
  | i = ID; EQ; v = value                     { `TypedId (i, v) }
  | i = ID                                    { `Id i       }
  | c = CONST                                 { `Const c    }
  | NULL                                      { `Null       } ;

obj_fields:
    obj = separated_list(COMMA, obj_field)    { obj } ;

obj_field:
    k = value; COLON; v = value              { (k, v) } ;

list_fields:
    vl = separated_list(COMMA, value)         { vl } ;
