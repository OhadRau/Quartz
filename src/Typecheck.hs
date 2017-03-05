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

type TypeEnv = Map.Map ValIdent Scheme

type Subst = Map.Map String Type

nullSubst :: Subst
nullSubst = Map.empty

compose :: Subst -> Subst -> Subst
s `compose` t = Map.map (subst s) t `Map.union` s

class Subst a where
  subst :: Subst -> a -> a
  freeTVars :: a -> Set.Set String

instance Subst Type where
  subst s t = case t of
    TVar a -> Map.findWithDefault t a s
    TApp t ts -> TApp t (map (subst s) ts)
    TArrow t u -> TArrow (subst s t) (subst s u)
    TImplicit n t ->
      let s' = n `Map.delete` s in
      TImplicit n (subst s' t)
    TRec n t ->
      let s' = n `Map.delete` s in
      TRec n (subst s' t)
    TDual t -> TDual $ subst s t
    TOffer s os ->
      let os' = map (\(n, i, r) -> (n, subst s i, subst s r)) os in
      TOffer s os'

  freeTVars t = case t of
    TRec tv t -> tv `Set.delete` freeTVars t
    TImplicit _ t -> freeTVars t
    TVar tv -> Set.singleton tv
    TApp _ ts -> Set.unions $ map freeTVars ts
    TArrow t u -> freeTVars t `Set.union` freeTVars u
    TDual t -> freeTVars t
    TOffer s os -> Set.unions $ map (\(_, i, r) -> freeTVars i `Set.union` freeTVars r) os

instance Subst Scheme where -- FIXME? Constraints treated wrong(?)
  subst s (Forall ts t) = Forall (map substConstraint ts) (subst s' t)
    where s' = foldr Map.delete s ts
          substConstraint (s, Nothing) = (s, Nothing)
          substConstraint (s, Just (CEqual t)) = (s, Just $ CEqual $ subst s' t)
          substConstraint (s, Just (CSubtype t)) = (s, Just $ CSubtype $ subst s' t)

  freeTVars (Forall [] t) = freeTVars t
  freeTVars (Forall ((v, Nothing):vs) t) = freeTVars (Forall vs t) `Set.delete` v
  freeTVars (Forall ((v, Just (CEqual c)):vs) t) = (freeTVars (Forall vs t) `Set.delete` v) `Set.union` freeTVars t
  freeTVars (Forall ((v, Just (CSubtype c)):vs) t) = (freeTVars (Forall vs t) `Set.delete` v) `Set.union` freeTvars t

letters :: [String]
letters = [1..] >>= flip replicateM ['a'..'z']

fresh :: Infer Type
fresh = do
  u <- get
  lift $ modify (+1)
  return $ TVar (letters !! u)

occurs :: String -> Type -> Bool
occurs tv t = tv `Set.member` freeTVars t

-- FIXME? Check for infinite types
bind :: String -> Type -> Infer Subst
bind a t | t == TVar a = return nullSubst
         | otherwise   = return $ Map.singleton a t

unify :: Type -> Type -> Infer Subst
unify (TVar a) t = bind a t
unify t (TVar a) = bind a t
unify t u = case (t, u) of
  (TRec tv t, TRec tv', t') -> do
    tvs <- bind tv (TVar tv')
    ts <- unify (apply tvs t) (apply tvs t')
    return $ ts `compose` tvs
  (TImplicit _ t, TImplicit _ t') -> unify t t'
  (TApp a ts, TApp b ts') ->
    if a == b
      then foldr (uncurry compose) nullSubst (zip ts ts')
      else throwError "Unification failed on TApp"
  (TArrow l r, TArrow l' r') -> do
    s1 <- unify l l'
    s2 <- unify (apply s1 r) (apply s1 r')
    return $ s2 `compose` s1
  (TDual t, TDual t') -> unify t t'
  (TOffer _ os, TOffer _ os') -> do
    let unifyOffers [] [] = nullSubst
        unifyOffers (_, i, r):os (_, i', r'):os' = do
          s1 <- unify i i'
          s2 <- unify (apply s1 r) (apply s1 r')
          s3 <- unifyOffers os os'
          return $ s3 `compose` s2 `compose` s1 in
    unifyOffers os os'
