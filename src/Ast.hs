module Ast where

import Data.List (intercalate)
import Data.Text (Text, unpack)

data ModIdent = MIdent [Text]
  deriving (Eq)

instance Show ModIdent where
  show (MIdent xs) = intercalate "." $ map unpack xs

data TypIdent = TIdent ModIdent Text
  deriving (Eq)

instance Show TypIdent where
  show (TIdent ms t) = show ms ++ "." ++ unpack t

data ValIdent = VIdent ModIdent Text
  deriving (Eq)

instance Show ValIdent where
  show (VIdent ms t) = show ms ++ "." ++ unpack t

data Type
  = TVar Text
  | TApp TypIdent [Type]
  | TArrow Type Type
  | TQuant Text Type (Maybe Constraint)
  | TRec Text Type
  | TDual Type
  | TOffer Type [(Text, Type, Type)]
  deriving (Eq)

showOffer (label, param, t) = unpack label ++ " : " ++ show param ++ " -> " ++ show t

instance Show Type where
  show (TVar tv) = unpack tv
  show (TApp t []) = show t
  show (TApp t ts) = show t ++ " " ++ intercalate " " (map show ts)
  show (TArrow t1 t2) = show t1 ++ " -> " ++ show t2
  show (TQuant tv t Nothing) = "forall " ++ unpack tv ++ "." ++ show t
  show (TQuant tv t (Just c)) = "forall " ++ unpack tv ++ show c ++ "." ++ show t
  show (TRec tv t) = "rec " ++ unpack tv ++ "." ++ show t
  show (TDual (TOffer t os)) = "+" ++ show t ++ "{" ++ intercalate "; " (map showOffer os) ++ "}"
  show (TOffer t os) = "&" ++ show t ++ "{" ++ intercalate "; " (map showOffer os) ++ "}"
  show (TDual t) = show t ++ "*"

-- Requires GHC 7.8+
-- pattern TConst id = TApp id []
-- pattern TTuple ts = TApp "," ts
-- pattern (:->) t1 t2 = TArrow t1 t2
-- pattern (:.) tv t = TQuant tv t Nothing
-- pattern (:=) (TQuant tv t _) t2 = TQuant tv t (CEqual t2)
-- pattern (:<) (TQuant tv t _) t2 = TQuant tv t (CSubtype t2)
-- pattern TChoose other offers = TDual (TOffer other offers)

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
normalize (TQuant n t c) = TQuant n (normalize t) (fmap normalizeConstraint c)
  where normalizeConstraint (CEqual t) = CEqual (normalize t)
        normalizeConstraint (CSubtype t) = CSubtype (normalize t)
normalize (TRec n t) = TRec n (normalize t)
normalize (TDual (TDual t)) = normalize t
normalize (TDual t) = TDual (normalize t)
normalize (TOffer p os) = TOffer (normalize p) (map normalizeOffer os)
  where normalizeOffer (n, c, t) = (n, normalize c, normalize t)
normalize t = t
