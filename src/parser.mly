%{
    open Ast
%}


%token QUOTE
%token LAMBDA
%token IF
%token SET
%token CALLCC

%token QUOTE_MARK
%token LEFT_PAREN
%token RIGHT_PAREN

%token <bool> BOOLEAN
%token <int> INTEGER
%token <string> IDENTIFIER

%token EOF

%start main
%type <Ast.expression option> main

%%

main:
    | e=expr EOF { Some e }
    | EOF { None }
;

expr:
    | b=BOOLEAN { ExprBoolean b }
    | i=INTEGER { ExprInteger i }
    | s=IDENTIFIER { ExprIdentifier s }
    (* '(1 2 3) *)
    | QUOTE_MARK; o=expr { ExprQuote o }
    (* (quote (1 2 3)) *)
    | LEFT_PAREN; QUOTE; o=expr; RIGHT_PAREN { ExprQuote o }
    (* (lambda (x1 x2 x3) e) *)
    | LEFT_PAREN; LAMBDA; LEFT_PAREN; p=params; RIGHT_PAREN; e=expr; RIGHT_PAREN { ExprLambda (p, e) }
    (* (if i t e) *)
    | LEFT_PAREN; IF; i=expr; t=expr; e=expr; RIGHT_PAREN { ExprIf (i, t, e) }
    (* (set! x e) *)
    | LEFT_PAREN; SET; i=IDENTIFIER; e=expr; RIGHT_PAREN { ExprSet (i, e) }
    (* (call/cc e) *)
    | LEFT_PAREN; CALLCC; e=expr; RIGHT_PAREN { ExprCallCC e }
    (* () *)
    | LEFT_PAREN; RIGHT_PAREN { ExprNil }
    (* (a b c) *)
    | LEFT_PAREN; h=expr; t=cdrs; RIGHT_PAREN { ExprCons (h, t) }
    ;

params:
    | (* empty *) { [] }
    | l=params; i=IDENTIFIER { List.rev (i::l) }
    ;
cdrs:
    | (* empty *) { ExprNil }
    | h=expr; t=cdrs { ExprCons (h, t) }
    ;
