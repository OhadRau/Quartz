module Parser where

import Text.Parsec
import Text.Parsec.Expr
import Text.Parsec.Token
import Text.Parsec.String

import Control.Monad.Identity
import Control.Applicative ((<$>), (<*), (*>))

import Ast ( ModIdent (..)
           , TypIdent (..)
           , ValIdent (..)
           , Constraint (..)
           , Type (..)
           , normalize
           )

langDefType :: LanguageDef ()
langDefType = LanguageDef
  { commentStart    = ""
  , commentEnd      = ""
  , commentLine     = ""
  , nestedComments  = False
  , identStart      = letter
  , identLetter     = alphaNum
  , opStart         = oneOf ".<>-+&=~*:;"
  , opLetter        = oneOf ".<>-+&=~*:;"
  , reservedNames   = ["forall", "rec"]
  , reservedOpNames = [".", "->", "~>", "&", "+", "*", "==", "<=", ":", ";"]
  , caseSensitive   = True
  }

lexType :: TokenParser ()
lexType = makeTokenParser langDefType

parensType :: Parser a -> Parser a
parensType = parens lexType

reservedType :: String -> Parser ()
reservedType = reserved lexType

reservedOpType :: String -> Parser ()
reservedOpType = reservedOp lexType

semiSepType :: Parser a -> Parser [a]
semiSepType = semiSep lexType

prefixOp :: String -> (a -> a) -> Operator String () Identity a
prefixOp s f = Prefix (reservedOpType s >> return f)

infixOp :: String -> (a -> a -> a) -> Assoc -> Operator String () Identity a
infixOp s f = Infix (reservedOpType s >> return f)

postfixOp :: String -> (a -> a) -> Operator String () Identity a
postfixOp s f = Postfix (reservedOpType s >> return f)

ident :: Parser String
ident = do
  first <- letter
  rest  <- many (letter <|> digit)
  return $ first:rest

mident :: Parser ModIdent
mident = MIdent <$> sepBy1 ident (char '.')

tident :: Parser TypIdent
tident = do
  mod <- mident
  char '.'
  typ <- ident
  return $ TIdent mod typ

vident :: Parser ValIdent
vident = do
  mod <- mident
  char '.'
  val <- ident
  return $ VIdent mod val

constraint :: Parser (Maybe Constraint)
constraint = optionMaybe (try eq <|> try st)
  where eq = CEqual   <$> (reservedOpType "==" *> exprType)
        st = CSubtype <$> (reservedOpType "<=" *> exprType)

exprTableType :: OperatorTable String () Identity Type
exprTableType = [ [ postfixOp "*" TDual
                  , infixOp "->" TArrow AssocRight ] ]

exprType :: Parser Type
exprType = buildExpressionParser exprTableType type'

type' :: Parser Type
type' = choice $ [parensType exprType] ++ map try [quant, rec, var, app, offer, choose]
  where var = TVar <$> ident
        app = do
          t <- tident
          ts <- many exprType
          return $ TApp t ts
        quant = do
          reservedType "forall"
          v <- ident
          c <- constraint
          reservedOpType "."
          t <- exprType
          return $ TQuant v t c
        rec = do
          reservedType "rec"
          v <- ident
          reservedOpType "."
          t <- exprType
          return $ TRec v t
        offer = do
          reservedOpType "&"
          target <- exprType
          offers <- between (char '{') (char '}') $ flip sepBy (reservedOpType ";") $ do
            name <- ident
            reservedOpType ":"
            ti <- exprType
            reservedOpType "~>"
            to <- exprType
            return $ (name, ti, to)
          return $ TOffer target offers
        choose = do
          reservedOpType "+"
          target <- exprType
          offers <- between (char '{') (char '}') $ flip sepBy (reservedOpType ";") $ do
            name <- ident
            reservedOpType ":"
            ti <- exprType
            reservedOpType "~>"
            to <- exprType
            return $ (name, ti, to)
          return $ TDual $ TOffer target offers

parseType :: String -> String -> Either ParseError Type
parseType name text = normalize <$> parse (contents exprType) name text
  where contents p = do
          whiteSpace lexType
          r <- p
          eof
          return r
