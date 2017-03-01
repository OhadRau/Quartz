module Ident where

import Data.List (intercalate)

data ModIdent = MIdent [String]
  deriving (Eq)

instance Show ModIdent where
  show (MIdent xs) = intercalate "." xs

data TypIdent = TIdent ModIdent String
  deriving (Eq)

instance Show TypIdent where
  show (TIdent ms t) = show ms ++ "." ++ t

data ValIdent = VIdent ModIdent String
  deriving (Eq)

instance Show ValIdent where
  show (VIdent ms t) = show ms ++ "." ++ t
