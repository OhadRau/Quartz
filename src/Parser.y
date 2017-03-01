{
{-# LANGUAGE GADTs #-}
{-# LANGUAGE NoMonomorphismRestriction #-}

module Parser where

import Ast
import Ident
import Token
}

%name quartz Ast
%tokentype { Token }
%error { parseError }

%token
  -- Keywords
  module   { TokenModule }
  require  { TokenRequire }
  let      { TokenLet }
  fun      { TokenFun }
  session  { TokenSession }
  protocol { TokenProtocol }
  branch   { TokenBranch }
  init     { TokenInit }
  do       { TokenDo }
  begin    { TokenBegin }
  end      { TokenEnd }
  if       { TokenIf }
  else     { TokenElse }
  case     { TokenCase }
  when     { TokenWhen }
  -- Special characters
  eol      { TokenEndOfLine }
  "("      { TokenOpenParen }
  ")"      { TokenCloseParen }
  "["      { TokenOpenBracket }
  "]"      { TokenCloseBracket }
  -- Operators
  "="      { TokenEqual }
  "=="     { TokenEqualTo }
  "<"      { TokenLessThan }
  ">"      { TokenGreaterThan }
  "<="     { TokenLessThanOrEqualTo }
  ">="     { TokenGreaterThanOrEqualTo }
  "!="     { TokenNotEqualTo }
  ":"      { TokenColon }
  "->"     { TokenArrow }
  "+"      { TokenPlus }
  "-"      { TokenMinus }
  "*"      { TokenTimes }
  "/"      { TokenDivide }
  ","      { TokenComma }
  "!"      { TokenSend }
  "."      { TokenDot }
  -- Terms
  var      { TokenVar $$ }
  mident   { TokenMident $$ }
  int      { TokenInt $$ }
  bool     { TokenBool $$ }
  decimal  { TokenDecimal $$ }
  string   { TokenString $$ }

%%

Ast : Tops { Ast $1 }

Vars
  : var      { [$1] }
  | var Vars { $1:$2 }

Top
  : module mident eol Tops end           { TModule $2 $4 }
  | require mident                       { TRequire $2 }
  | let var "=" Expr                     { TLet $2 $4 }
  | fun var Vars eol Exprs end           { let fn = foldr ELambda (ESequence $5) $3 in
                                           TLet $2 fn } 
  | session var eol TopSessions end      { TLet $2 (ESession $4) }
  | session var Vars eol TopSessions end { let fn = foldr ELambda (ESession $5) $3 in
                                           TLet $2 fn }

Tops
  : Top eol      { [$1] }
  | Top eol Tops { $1:$3 }

Expr
  : bool                                 { ELiteral (LBool $1) }
  | int                                  { ELiteral (LInteger $1) }
  | decimal                              { ELiteral (LDecimal $1) }
  | string                               { ELiteral (LString $1) }
  | var                                  { EIdent (VIdent (MIdent []) $1) }
  | let var "=" Expr eol Exprs           { ELet $2 $4 (ESequence $6) }
  | fun var Vars eol Exprs end eol Exprs { let fn = foldr ELambda (ESequence $5) $3 in
                                           ELet $2 fn (ESequence $8) }

Exprs
  : Expr eol       { [$1] }
  | Expr eol Exprs { $1:$3 }

TopSession
  : module mident eol TopSessions end    { TModule $2 $4 }
  | require mident                       { TRequire $2 }
  | let var "=" ExprSession              { TLet $2 $4 }
  | fun var Vars eol ExprSessions end    { let fn = foldr ELambda (ESequence $5) $3 in
                                           TLet $2 fn } 
  | session var eol TopSessions end      { TLet $2 (ESession $4) }
  | session var Vars eol TopSessions end { let fn = foldr ELambda (ESession $5) $3 in
                                           TLet $2 fn }
  | init eol ExprSessions end            { TInit (ESequence $3) }
  | branch var eol ExprSessions end      { TBranch $2 [] (ESequence $4) }
  | branch var Vars eol ExprSessions end { TBranch $2 $3 (ESequence $5) }

TopSessions
  : TopSession eol             { [$1] }
  | TopSession eol TopSessions { $1:$3 }

ExprSession
  : bool                                               { ELiteral (LBool $1) }
  | int                                                { ELiteral (LInteger $1) }
  | decimal                                            { ELiteral (LDecimal $1) }
  | string                                             { ELiteral (LString $1) }
  | var                                                { EIdent (VIdent (MIdent []) $1) }
  | let var "=" ExprSession eol ExprSessions           { ELet $2 $4 (ESequence $6) }
  | fun var Vars eol ExprSessions end eol ExprSessions { let fn = foldr ELambda (ESequence $5) $3 in
                                                         ELet $2 fn (ESequence $8) }

ExprSessions
  : ExprSession eol              { [$1] }
  | ExprSession eol ExprSessions { $1:$3 }
