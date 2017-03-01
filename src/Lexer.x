{
module Lexer where

import Token
}

%wrapper "basic"

$digit = 0-9
$lower = [a-z]
$upper = [A-Z]
$alpha = [a-zA-Z]
$eol   = [\n]
$white = [\ \t\f\v\r]

tokens :-
  -- Whitespace
  $eol+                   { \s -> TokenEndOfLine }
  $white+                 ;

  -- Comments
  "#".*                   ;

  -- Syntax
  module                  { \s -> TokenModule }
  require                 { \s -> TokenRequire }
  let                     { \s -> TokenLet }
  fun                     { \s -> TokenFun }
  session                 { \s -> TokenSession }
  protocol                { \s -> TokenProtocol }
  branch                  { \s -> TokenBranch }
  init                    { \s -> TokenInit }
  do                      { \s -> TokenDo }
  begin                   { \s -> TokenBegin }
  end                     { \s -> TokenEnd }
  if                      { \s -> TokenIf }
  else                    { \s -> TokenElse }
  case                    { \s -> TokenCase }
  when                    { \s -> TokenWhen }
  
  \(                      { \s -> TokenOpenParen }
  \)                      { \s -> TokenCloseParen }
  \[                      { \s -> TokenOpenBracket }
  \]                      { \s -> TokenCloseBracket }
  
  $lower [$alpha $digit]* { \s -> TokenVar s }
  $upper [$alpha $digit]* { \s -> TokenMident s }
  $digit+                 { \s -> TokenInt (read s) }
  $digit+ \. $digit+      { \s -> TokenDecimal (read s) }

{
scanTokens :: String -> [Token]
scanTokens = alexScanTokens
}
