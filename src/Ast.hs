module Ast where

import Data.List (intercalate)

import Type (Type)
import Ident (ModIdent, TypIdent, ValIdent)

data Normal  -- In a normal context
data Session -- In a session context

newtype Ast = Ast [Top]

instance Show Ast where
  show (Ast ts) = intercalate "\n\n" $ map show ts

data Literal
  = LBool Bool
  | LInteger Int
  | LDecimal Double
  | LString String

instance Show Literal where
  show (LBool b) = show b
  show (LInteger i) = show i
  show (LDecimal d) = show d
  show (LString s) = show s

data Expr a where
  -- 1, "hi", True, 3.5
  ELiteral :: Literal -> Expr a
  -- id, doThing
  EIdent :: ValIdent -> Expr a
  -- let x = e in z
  ELet :: String -> [Expr a] -> [Expr a] -> Expr a
  -- session ... end
  ESession :: [Expr Session] -> Expr a
  -- if x then y else z end
  ECond :: [Expr a] -> [Expr a] -> [Expr a] -> Expr a
  -- |x, y| e end
  ELambda :: [String] -> [Expr a] -> Expr a
  -- f(x, y)
  EApply :: Expr a -> [Expr a] -> Expr a
  -- branch F(x, y) from c ... end
  EBranch :: String -> [String] -> Expr Session -> [Expr Session] -> Expr Session
  -- c!Hello("server") ... es
  EChoose :: Expr Session -> String -> [Expr Session] -> [Expr Session] -> Expr Session
  -- spawn S(1, "hi")
  ESpawn :: ValIdent -> [Expr a] -> Expr a

instance forall a. Show (Expr a) where
  show (ELiteral l) = show l
  show (EIdent i) = show i
  show (ELet n v c) = "let " ++ n ++ " = " ++ show v ++ "\n" ++ show c
  show (ESession s) = "session\n" ++ intercalate "\n" (map show s) ++ "\nend"
  show (ECond p t f) = "if " ++ show p ++ "\n" ++ show t ++ "\n" ++ show f ++ "\nend"
  show (ELambda vs e) = "do |" ++ intercalate "," (map show vs) ++ "|\n" ++ show e ++ "\nend"
  show (EApply f x) = show f ++ " " ++ show x
  show (EBranch n vs tgt e) = let vs' = intercalate " " (map show vs) in
                              "branch " ++ n ++ " " ++ vs' ++ " from " ++ show tgt ++ "\n" ++ show e ++ "\nend"
  show (EChoose tgt msg) = show tgt ++ "!" ++ msg
  show (ESpawn n) = show n

data Top where
  TModule :: String -> [Top] -> Top
  TRequire :: String -> Top
  TLet :: String -> Expr Normal -> Top

instance Show Top where
  show (TModule n ts) = "module " ++ n ++ "\n" ++ intercalate "\n" (map show ts) ++ "\nend"
  show (TRequire m) = "require " ++ m
  show (TLet n v) = "let " ++ n ++ " = " ++ show v
