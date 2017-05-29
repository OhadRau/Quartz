%{
open Ast
open Pos

let mk_ast desc pos =
  { ast_desc = desc
  ; ast_src = None
  ; ast_pos = pos
  }

let mk_stmt desc pos =
  { stmt_desc = desc
  ; stmt_type = Nothing
  ; stmt_src = None
  ; stmt_pos = pos
  }

let mk_expr desc pos =
  { expr_desc = Left desc
  ; expr_type = Nothing
  ; expr_src = None
  ; expr_pos = pos
  }

let mk_sesh desc pos =
  { expr_desc = Right desc
  ; expr_type = Nothing
  ; expr_src = None
  ; expr_pos = pos
  }
%}

%token AT ASSIGN CLOSE CLOSE_PAREN COMMA ELSE END EOF EQUALS FROM FUN IF LET LOOP MODULE DELIMIT ON OPEN_PAREN OR PIPE REQUIRE SEND SESSION SPAWN
%token <string> IDENT STRING
%token <int * int> NUMBER

%start ast
%type <[`No] Ast.ast> ast

%%

ast:
  | stmts EOF
    { mk_ast $1 (mk_pos $startpos) }
;

stmts:
  | stmt DELIMIT
    { [$1] }
  | stmt DELIMIT stmts
    { $1::$3 }
;

stmt:
  | REQUIRE IDENT
    { mk_stmt (SOpen (Value $2)) (mk_pos $startpos) }
  | MODULE IDENT DELIMIT stmts END
    { mk_stmt (SModule ($2, $4)) (mk_pos $startpos) }
  | LET IDENT EQUALS DELIMIT? exprs
    { mk_stmt (SLet ($2, $5)) (mk_pos $startpos) }
  | FUN IDENT OPEN_PAREN params CLOSE_PAREN DELIMIT exprs END
    { let lam = mk_expr (ELam ($4, $7)) (mk_pos $startpos) in
      mk_stmt (SLet ($2, [lam])) (mk_pos $startpos) }
  | SESSION IDENT OPEN_PAREN params CLOSE_PAREN DELIMIT sessions END
    { let sesh = mk_expr (ESession $7) (mk_pos $startpos) in
      let lam = mk_expr (ELam ($4, [sesh])) (mk_pos $startpos) in
      mk_stmt (SLet ($2, [lam])) (mk_pos $startpos) }
;

params:
  | separated_list(COMMA, IDENT)
    { $1 }
;

exprs:
  | expr DELIMIT
    { [$1] }
  | expr DELIMIT exprs
    { $1::$3 }
;

sessions:
  | session DELIMIT
    { [$1] }
  | session DELIMIT sessions
    { $1::$3 }
;

expr:
  | IDENT
    { mk_expr (ELiteral (Number (0, 0))) (mk_pos $startpos) }
;

session:
  | expr
    { Obj.magic $1 }
;
