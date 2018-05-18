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

  let loc_annot start_pos end_pos expr = (expr, { start_pos; end_pos; })
%}

%start <Location.t Ast.expression option> prog

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
    ExprVar(ref) |> loc_annot $symbolstartpos $endpos
  }
  | ref = iv_identifier          {
    ExprIVar(ref) |> loc_annot $symbolstartpos $endpos
  }
  | c = CONST                    {
    let sub_expr = ExprValue(Nil) |> loc_annot $symbolstartpos $endpos in
    ExprConst((c, Any), sub_expr) |> loc_annot $symbolstartpos $endpos
  }
  | id = ID EQ v = rhs_assign    {
    ExprAssign(id, v) |> loc_annot $symbolstartpos $endpos
  }
  | id = IVAR EQ v = rhs_assign  {
    ExprIVarAssign(id, v) |> loc_annot $symbolstartpos $endpos
  }
  | c = CONST EQ v = rhs_assign  {
    ExprConstAssign(c, v) |> loc_annot $symbolstartpos $endpos
  }
  | p = primitive                {
    ExprValue(p) |> loc_annot $symbolstartpos $endpos
  }
  | e = expr                     { e }
  ;

rhs_assign:
  | s = statement { s }
  ;

expr:
  | c = command_call { c }
  | LAMBDA l = lambda { l }
  | f = func     { f }
  ;

command_call:
  c = command { c } ;

command:
  | m = method_call args = command_args {
    let sub_expr = ExprValue(Nil) |> loc_annot $symbolstartpos $endpos in
    ExprCall(sub_expr, m, args)   |> loc_annot $symbolstartpos $endpos
  }
  | m = ID args = command_args {
    let sub_expr = ExprValue(Nil) |> loc_annot $symbolstartpos $endpos in
    ExprCall(sub_expr, m, args)   |> loc_annot $symbolstartpos $endpos
  }
  | c1 = identifier call_op c2 = method_call {
    let sub_expr = ExprVar(c1) |> loc_annot $symbolstartpos $endpos in
    ExprCall(sub_expr, c2, []) |> loc_annot $symbolstartpos $endpos
  }
  | c1 = identifier call_op c2 = method_call args = command_args {
    let sub_expr = ExprVar(c1)   |> loc_annot $symbolstartpos $endpos in
    ExprCall(sub_expr, c2, args) |> loc_annot $symbolstartpos $endpos
  }
  ;

call_op: DOT { } ;

method_call:
  id = FID { id } ;

command_args:
  | node = command_call { [node] }
  | args = call_args { args }
  ;

identifier:
  id = ID { id, Any } ;

iv_identifier:
  iv = IVAR { iv, Any } ;

func:
  | DEF fn = ID args = fn_args EOS? END {
    let body = ExprValue(Nil) |> loc_annot $symbolstartpos $endpos in
    ExprFunc(fn, args, body) |> loc_annot $symbolstartpos $endpos
  }
  | DEF fn = ID args = fn_args EOS? s = statement statement_end? END {
    ExprFunc(fn, args, s) |> loc_annot $symbolstartpos $endpos
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
  ;

lambda:
  | body = lambda_body {
    ExprLambda ([], body)   |> loc_annot $symbolstartpos $endpos
  }
  | args = fn_args body = lambda_body {
    ExprLambda (args, body) |> loc_annot $symbolstartpos $endpos
  }
  ;

lambda_body:
  | LAMBEG s = statement statement_end? RBRACE {
    s
  }
  | LAMBEG RBRACE {
    ExprValue(Nil) |> loc_annot $symbolstartpos $endpos
  }
  ;

fn_args:
  LPAREN p = separated_list(COMMA, identifier) RPAREN { p } ;

call_args:
  LPAREN p = separated_list(COMMA, statement) RPAREN { p } ;

obj_fields:
  obj = separated_list(COMMA, obj_field)    { obj } ;

obj_field:
  k = primitive COLON v = primitive         { k, v } ;

list_fields:
  vl = separated_list(COMMA, primitive)     { vl } ;
