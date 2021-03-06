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

%token CLOSE CLOSE_PAREN COMMA ELSE END EOF EQUALS FALSE FROM FUN IF LET LOOP MODULE DELIMIT ON OPEN_PAREN OR PIPE REQUIRE SEND SESSION SPAWN TRUE
%token <string> IDENT STRING
%token <int * int> NUMBER

%nonassoc below_DELIMIT
%nonassoc DELIMIT

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
    { mk_stmt (SRequire (Value $2)) (mk_pos $startpos) }
  | MODULE IDENT DELIMIT stmts END
    { mk_stmt (SModule ($2, $4)) (mk_pos $startpos) }
  | LET IDENT EQUALS expr
    { mk_stmt (SLet ($2, [$4])) (mk_pos $startpos) }
  | LET IDENT EQUALS DELIMIT exprs END
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
  | expr %prec below_DELIMIT
    { [$1] }
  | expr DELIMIT
    { [$1] }
  | expr DELIMIT exprs
    { $1::$3 }
;

sessions:
  | session %prec below_DELIMIT
    { [$1] }
  | session DELIMIT
    { [$1] }
  | session DELIMIT sessions
    { $1::$3 }
;

expr:
  | NUMBER
    { mk_expr (ELiteral (Number $1)) (mk_pos $startpos) }
  | TRUE
    { mk_expr (ELiteral (Bool true)) (mk_pos $startpos) }
  | FALSE
    { mk_expr (ELiteral (Bool false)) (mk_pos $startpos) }
  | STRING
    { mk_expr (ELiteral (String $1)) (mk_pos $startpos) }
  | IDENT
    { mk_expr (EIdent (Value $1)) (mk_pos $startpos) }
  | LET IDENT EQUALS expr DELIMIT exprs
    { mk_expr (ELet ($2, [$4], $6)) (mk_pos $startpos) }
  | LET IDENT EQUALS DELIMIT exprs END DELIMIT exprs
    { mk_expr (ELet ($2, $5, $8)) (mk_pos $startpos) }
  | FUN IDENT OPEN_PAREN params CLOSE_PAREN DELIMIT exprs END DELIMIT exprs
    { let lam = mk_expr (ELam ($4, $7)) (mk_pos $startpos) in
      mk_expr (ELet ($2, [lam], $10)) (mk_pos $startpos) }
  | SESSION IDENT OPEN_PAREN params CLOSE_PAREN DELIMIT sessions END DELIMIT exprs
    { let sesh = mk_expr (ESession $7) (mk_pos $startpos) in
      let lam = mk_expr (ELam ($4, [sesh])) (mk_pos $startpos) in
      mk_expr (ELet ($2, [lam], $10)) (mk_pos $startpos) }
  | PIPE params PIPE expr
    { mk_expr (ELam ($2, [$4])) (mk_pos $startpos) }
  | PIPE params PIPE DELIMIT exprs END
    { mk_expr (ELam ($2, $5)) (mk_pos $startpos) }
  | expr OPEN_PAREN separated_list(COMMA, expr) CLOSE_PAREN
    { mk_expr (EApp ($1, $3)) (mk_pos $startpos) }
  | IF expr DELIMIT exprs ELSE DELIMIT exprs END
    { mk_expr (ECond ($2, $4, $7)) (mk_pos $startpos) }
  | SPAWN IDENT OPEN_PAREN separated_list(COMMA, expr) CLOSE_PAREN
    { mk_expr (ESpawn (Value $2, $4)) (mk_pos $startpos) }
  /* No rule for parsing sessions, since anonymous sessions
     are not yet supported */
;

session:
  | expr
    { Obj.magic $1 }
  |  CLOSE
    { mk_sesh EClose (mk_pos $startpos) }
  | LOOP
    { mk_sesh (ELoop None) (mk_pos $startpos) }
  | LOOP OPEN_PAREN separated_list(COMMA, session) CLOSE_PAREN
    { mk_sesh (ELoop (Some $3)) (mk_pos $startpos) }
  | IDENT SEND IDENT DELIMIT sessions
    { mk_sesh (ESend (Value $1, $3, [], $5)) (mk_pos $startpos) }
  | IDENT SEND IDENT OPEN_PAREN separated_list(COMMA, session) CLOSE_PAREN DELIMIT sessions
    { mk_sesh (ESend (Value $1, $3, $5, $8)) (mk_pos $startpos) }
  | ON IDENT FROM IDENT DELIMIT sessions or_branches
    { mk_sesh (EBranch ($4, ($2, [], $6)::$7)) (mk_pos $startpos) }
  | ON IDENT OPEN_PAREN separated_list(COMMA, IDENT) CLOSE_PAREN FROM IDENT DELIMIT sessions or_branches
    { mk_sesh (EBranch ($7, ($2, $4, $9)::$10)) (mk_pos $startpos) }
;

or_branches:
  | END
    { [] }
  | OR IDENT DELIMIT sessions or_branches
    { ($2, [], $4)::$5 }
  | OR IDENT OPEN_PAREN separated_list(COMMA, IDENT) CLOSE_PAREN DELIMIT sessions or_branches
    { ($2, $4, $7)::$8 }
;
