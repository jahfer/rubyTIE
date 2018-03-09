%token <int> INT
%token <float> FLOAT
%token <string> STRING
%token TRUE FALSE NIL
%token LBRACE RBRACE LBRACK RBRACK LPAREN RPAREN LAMBEG
%token COLON COMMA
%token EOS EOF
%token <string> ID FID IVAR
%token <string> CONST
%token EQ DEF END LAMBDA DOT

%{
  open Ast
  open Location

  let with_loc start_pos end_pos expr_desc =
    let expr_loc = { start_pos; end_pos; } in
    { expr_loc; expr_desc }
%}

%start <Ast.core_expression option> prog

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
  | ref = identifier             {
    ExprVar(ref) |> with_loc $symbolstartpos $endpos
  }
  | ref = iv_identifier          {
    ExprIVar(ref) |> with_loc $symbolstartpos $endpos
  }
  | c = CONST                    {
    let sub_expr = ExprValue(Nil) |> with_loc $symbolstartpos $endpos in
    ExprConst((c, Any), sub_expr) |> with_loc $symbolstartpos $endpos
  }
  | id = ID EQ v = rhs_assign    {
    ExprAssign(id, v) |> with_loc $symbolstartpos $endpos
  }
  | id = IVAR EQ v = rhs_assign  {
    ExprIVarAssign(id, v) |> with_loc $symbolstartpos $endpos
  }
  | c = CONST EQ v = rhs_assign  {
    ExprConstAssign(c, v) |> with_loc $symbolstartpos $endpos
  }
  | p = primitive                {
    ExprValue(p) |> with_loc $symbolstartpos $endpos
  }
  | e = expr                     { e }
  ;

rhs_assign:
  | s = statement { s }
  ;

expr:
  | c = command_call { c }
  | f = func     { f }
  ;

command_call:
  c = command { c } ;

command:
  | c = method_call args = command_args {
    let sub_expr = ExprValue(Nil) |> with_loc $symbolstartpos $endpos in
    ExprCall(sub_expr, c, args) |> with_loc $symbolstartpos $endpos
  }
  | c1 = identifier call_op c2 = method_call {
    let sub_expr = ExprVar(c1) |> with_loc $symbolstartpos $endpos in
    ExprCall(sub_expr, c2, []) |> with_loc $symbolstartpos $endpos
  }
  | c1 = identifier call_op c2 = method_call args = command_args {
    let sub_expr = ExprVar(c1) |> with_loc $symbolstartpos $endpos in
    ExprCall(sub_expr, c2, args) |> with_loc $symbolstartpos $endpos
  }
  ;

call_op: DOT { } ;

method_call:
  id = FID { id } ;

command_args:
  | node = command_call { [node] }
  | args = fn_args {
    args |> List.map (fun x ->
      ExprVar(x) |> with_loc $symbolstartpos $endpos)
  }
  ;

identifier:
  id = ID { id, Any } ;

iv_identifier:
  iv = IVAR { iv, Any } ;

func:
  | DEF fn = ID args = fn_args EOS? END {
    let body = ExprValue(Nil) |> with_loc $symbolstartpos $endpos in
    ExprFunc(fn, args, body) |> with_loc $symbolstartpos $endpos
  }
  | DEF fn = ID args = fn_args EOS? s = statement statement_end? END {
    ExprFunc(fn, args, s) |> with_loc $symbolstartpos $endpos
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
  | body = lambda_body                { Lambda ([], body) }
  | args = fn_args body = lambda_body { Lambda (args, body) }
  ;

lambda_body:
  | LAMBEG s = statement statement_end? RBRACE {
    s
  }
  | LAMBEG RBRACE {
    ExprValue(Nil) |> with_loc $symbolstartpos $endpos
  }
  ;

fn_args:
  LPAREN p = separated_list(COMMA, identifier) RPAREN { p } ;

obj_fields:
  obj = separated_list(COMMA, obj_field)    { obj } ;

obj_field:
  k = primitive COLON v = primitive         { k, v } ;

list_fields:
  vl = separated_list(COMMA, primitive)     { vl } ;
