module Main where

import Prelude hiding (readFile, putStrLn)
import Data.Text
import Data.Text.IO
import System.Environment

import Ast

compileFile :: FilePath -> IO ()
compileFile file = readFile file >>= putStrLn

myType :: Type
myType =
  TRec "self" $
    TQuant "a" (
      TQuant "b" (
        TRec "talkingToA" $
          TOffer (TVar "a")
            [ ("TalkToMe", tUnit, TVar "talkingToA")
            , ("TalkToB", tUnit, TOffer (TVar "b") [("Value", tInt, tEps)])
            ]
      ) (Just $ CSubtype $ TDual $ TVar "self")
    ) (Just $ CSubtype $ TDual $ TVar "self")
  where tUnit = TApp (TIdent (MIdent ["Prelude"]) "()") []
        tInt = TApp (TIdent (MIdent ["Prelude"]) "Int") []
        tEps = TApp (TIdent (MIdent ["Prelude"]) "Eps") []

main :: IO ()
main = do
  args <- getArgs
  mapM_ compileFile args
  print myType