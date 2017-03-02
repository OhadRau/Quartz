module Typecheck where

import Control.Monad
import Control.Monad.State
import Control.Monad.Except
import qualified Data.Map as Map
import qualified Data.Set as Set
import Control.Applicative ((<$>))

import Ast
import Type
import Ident

type Infer a = ExceptT String (State Int) a

runInfer :: Infer (Subst, Type) -> Either String Type
runInfer m = snd <$> evalState (runExceptT m) 0

type TypeEnv = Map.Map ValIdent Type

type Subst = Map.Map String Type

nullSubst :: Subst
nullSubst = Map.empty

compose :: Subst -> Subst -> Subst
s `compose` t = Map.map (subst s) t `Map.union` s

subst :: Subst -> Type -> Type
subst s t = case t of
  TVar a -> Map.findWithDefault t a s
  TApp t ts -> TApp t (map (subst s) ts)
  TArrow t u -> TArrow (subst s t) (subst s u)
  TQuant n t Nothing ->
    let s' = n `Map.delete` s in
    TQuant n (subst s' t) Nothing
  TQuant n t (Just (CEqual c)) ->
    let s' = n `Map.delete` s in
    TQuant n (subst s' t) (Just (CEqual (subst s' c)))
  TQuant n t (Just (CSubtype c)) ->
    let s' = n `Map.delete` s in
    TQuant n (subst s' t) (Just (CSubtype (subst s' c)))
  TRec n t ->
    let s' = n `Map.delete` s in
    TRec n (subst s' t)
  TDual t -> TDual $ subst s t
  TOffer t os ->
    let os' = map (\(n, i, r) -> (n, subst s i, subst s r)) os in
    TOffer (subst s t) os

freeTVars :: Type -> Set.Set String
freeTVars t = case t of
  TRec tv t -> tv `Set.delete` freeTVars t
  TQuant tv t Nothing -> tv `Set.delete` freeTVars t
  TQuant tv t (Just (CEqual c)) -> tv `Set.delete` (freeTVars t `Set.union` freeTVars c)
  TQuant tv t (Just (CSubtype c)) -> tv `Set.delete` (freeTVars t `Set.union` freeTVars c)
  TVar tv -> Set.singleton tv
  TApp _ ts -> Set.unions $ map freeTVars ts
  TArrow t u -> freeTVars t `Set.union` freeTVars u
  TDual t -> freeTVars t
  TOffer t os ->
    let freeOffers = Set.unions $ map (\(_, i, r) -> freeTVars i `Set.union` freeTVars r) os in
    freeTVars t `Set.union` freeOffers

letters :: [String]
letters = [1..] >>= flip replicateM ['a'..'z']

fresh :: Infer Type
fresh = do
  u <- get
  lift $ modify (+1)
  return $ TVar (letters !! u)

occurs :: String -> Type -> Bool
occurs tv t = tv `Set.member` freeTVars t

unify :: Type -> Type -> Infer Subst
unify t u = undefined
