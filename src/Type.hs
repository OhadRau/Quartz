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
  deriving (Eq)

showOffer (label, param, t) = label ++ " : " ++ show param ++ " ~> " ++ show t

instance Show Type where
  show (TVar tv) = tv
  show (TApp t []) = show t
  show (TApp t ts) = show t ++ " " ++ intercalate " " (map show ts)
  show (TArrow t1 t2) = show t1 ++ " -> " ++ show t2
  show (TImplicit s t u) = "[" ++ s ++ " : " ++ show t ++ "] " ++ show u
  show (TNamed s t) = "(" ++ s ++ " : " ++ show t ++ ")"
  show (TRec tv t) = "rec " ++ tv ++ "." ++ show t
  show (TChoose t os) = "+" ++ t ++ "{" ++ intercalate "; " (map showOffer os) ++ "}"
  show (TOffer t os) = "&" ++ t ++ "{" ++ intercalate "; " (map showOffer os) ++ "}"
  show (TDual t) = show t ++ "*"

pattern TTuple ts = TApp (TIdent (MIdent []) ",") ts
pattern (:->) t1 t2 = TArrow t1 t2
pattern TChoose target offers = TDual (TOffer target offers)

data Constraint
  = CEqual Type
  | CSubtype Type
  deriving (Eq)

instance Show Constraint where
  show (CEqual t) = " == " ++ show t
  show (CSubtype t) = " <= " ++ show t

normalize :: Type -> Type
normalize (TApp a ts) = TApp a (map normalize ts)
normalize (TArrow t1 t2) = TArrow (normalize t1) (normalize t2)
normalize (TImplicit s t u) = TImplicit s (normalize t) (normalize u)
normalize (TRec n t) = TRec n (normalize t)
normalize (TDual (TDual t)) = normalize t
normalize (TDual t) = TDual (normalize t)
normalize (TOffer p os) = TOffer p (map normalizeOffer os)
  where normalizeOffer (n, c, t) = (n, normalize c, normalize t)
normalize t = t

normalizeScheme :: Scheme -> Scheme
normalizeScheme (Forall ts t) = Forall (map normalizeConstraint ts) (normalize t)
  where normalizeConstraint (s, Just (CEqual t)) = (s, Just (CEqual $ normalize t))
        normalizeConstraint (s, Just (CSubtype t)) = (s, Just (CSubtype $ normalize t))
        normalizeConstraint c = c

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
