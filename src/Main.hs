module Main where

import Prelude
import System.Environment

import Ast
import Type
import Ident
import Lexer (scanTokens)
import Parser (quartz)

compileFile :: FilePath -> IO ()
compileFile file = do
  contents <- readFile file
  print $ quartz $ scanTokens contents

main :: IO ()
main = do
  args <- getArgs
  mapM_ compileFile args
