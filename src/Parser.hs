module Parser where

import Text.Parsec.Prim
import qualified Data.Text as T

parseType :: GenParser T.Text Ast.Type
parseType = undefined
