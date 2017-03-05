module Ast where

import Data.List (intercalate)

import Type (Type)
import Ident (ModIdent, TypIdent, ValIdent)

data Normal  -- In a normal context
data Session -- In a session context

newtype Ast = Ast [Top Normal]

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
  ELiteral :: Literal -> Expr a
  EIdent :: ValIdent -> Expr a
  ELet :: String -> Expr a -> Expr a -> Expr a
  ESession :: [Top Session] -> Expr a
  ECond :: Expr a -> Expr a -> Expr a -> Expr a
  ELambda :: String -> Expr a -> Expr a
  EApply :: Expr a -> Expr a -> Expr a
  EBranch :: String -> [String] -> Expr Session -> Expr Session
  ESpawn :: ValIdent -> Expr a
  ESequence :: [Expr a] -> Expr a

instance forall a. Show (Expr a) where
  show (ELiteral l) = show l
  show (EIdent i) = show i
  show (ELet n v c) = "let " ++ n ++ " = " ++ show v ++ "\n" ++ show c
  show (ESession s) = "session\n" ++ intercalate "\n" (map show s) ++ "\nend"
  show (ECond p t f) = "if " ++ show p ++ "\n" ++ show t ++ "\n" ++ show f ++ "\nend"
  show (ELambda v e) = "do |" ++ v ++ "|\n" ++ show e ++ "\nend"
  show (EApply f x) = show f ++ " " ++ show x
  show (EBranch n vs e) = let vs' = intercalate " " (map show vs) in
                          "branch " ++ n ++ " " ++ vs' ++ "\n" ++ show e ++ "\nend"
  show (ESpawn n) = show n
  show (ESequence es) = "begin\n" ++ intercalate "\n" (map show es) ++ "\nend"

data Top a where
  TModule :: String -> [Top a] -> Top a
  TRequire :: String -> Top a
  TLet :: String -> Expr a -> Top a
  TInit :: Expr Session -> Top Session
  TBranch :: String -> [String] -> Expr Session -> Top Session

instance forall a. Show (Top a) where
  show (TModule n ts) = "module " ++ n ++ "\n" ++ intercalate "\n" (map show ts) ++ "\nend"
  show (TRequire m) = "require " ++ m
  show (TLet n v) = "let " ++ n ++ " = " ++ show v
  show (TInit e) = "init\n" ++ show e ++ "\nend"
  show (TBranch n vs e) = let vs' = intercalate " " (map show vs) in
                          "branch " ++ n ++ " " ++ vs' ++ "\n" ++ show e ++ "\nend"
