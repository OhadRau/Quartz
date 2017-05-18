module Type where

import Data.List (intercalate)

import Ident

data Scheme = Forall [(String, Maybe Constraint)] Type
  deriving (Show, Eq)

data Type
  = TVar String
  | TApp TypIdent [Type]
  | TArrow Type Type
  | TImplicit String Type Type
  | TNamed String Type
  | TRec String Type
  | TDual Type
  | TOffer String [(String, Type, Type)]
  | TChoose String [(String, Type, Type)]
  deriving (Eq)

showBranch (label, param, t) = label ++ " : " ++ show param ++ " ~> " ++ show t

instance Show Type where
  show (TVar tv) = tv
  show (TApp t []) = show t
  show (TApp t ts) = show t ++ " " ++ intercalate " " (map show ts)
  show (TArrow t1 t2) = show t1 ++ " -> " ++ show t2
  show (TImplicit s t u) = "[" ++ s ++ " : " ++ show t ++ "] " ++ show u
  show (TNamed s t) = "(" ++ s ++ " : " ++ show t ++ ")"
  show (TRec tv t) = "rec " ++ tv ++ "." ++ show t
  show (TChoose t os) = "+" ++ t ++ "{" ++ intercalate "; " (map showBranch os) ++ "}"
  show (TOffer t os) = "&" ++ t ++ "{" ++ intercalate "; " (map showBranch os) ++ "}"
  show (TDual t) = show t ++ "*"

pattern TTuple ts = TApp (TIdent (MIdent []) ",") ts
pattern (:->) t1 t2 = TArrow t1 t2

data Constraint
  = CEqual Type
  | CSubtype Type
  deriving (Eq)

instance Show Constraint where
  show (CEqual t) = " == " ++ show t
  show (CSubtype t) = " <= " ++ show t

-- Define common types
preludeType :: String -> [Type] -> Type
preludeType t = TApp (TIdent (MIdent ["Prelude"]) t)

tInt :: Type
tInt = preludeType "Int" []

tBool :: Type
tBool = preludeType "Bool" []

tFloat :: Type
tFloat = preludeType "Float" []

tString :: Type
tString = preludeType "String" []
