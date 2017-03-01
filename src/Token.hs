module Token where

data Token
  -- Keywords
  = TokenModule
  | TokenRequire
  | TokenLet
  | TokenFun
  | TokenSession
  | TokenProtocol
  | TokenBranch
  | TokenInit
  | TokenDo
  | TokenBegin
  | TokenEnd
  | TokenIf
  | TokenElse
  | TokenCase
  | TokenWhen
  -- Special characters
  | TokenEndOfLine
  | TokenOpenParen
  | TokenCloseParen
  | TokenOpenBracket
  | TokenCloseBracket
  -- Operators
  | TokenEqual
  | TokenEqualTo
  | TokenLessThan
  | TokenGreaterThan
  | TokenLessThanOrEqualTo
  | TokenGreaterThanOrEqualTo
  | TokenNotEqualTo
  | TokenColon
  | TokenArrow
  | TokenPlus
  | TokenMinus
  | TokenTimes
  | TokenDivide
  | TokenComma
  | TokenSend
  | TokenDot
  -- Terms
  | TokenVar String
  | TokenMident String
  | TokenInt Int
  | TokenBool Bool
  | TokenDecimal Double
  | TokenString String
  deriving (Eq, Show)

parseError :: [Token] -> a
parseError ts = error $ "Parse error: " ++ show ts
