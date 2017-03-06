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

lookupEnv :: TypeEnv -> ValIdent -> (Subst, Type)
lookupEnv env x = case Map.lookup x env of
  Nothing -> throwError $ "Unbound variable: " ++ show x
  Just s -> do
    t <- instantiate s
    return (nullSubst, t)

type Subst = Map.Map String Type

nullSubst :: Subst
nullSubst = Map.empty

compose :: Subst -> Subst -> Subst
s `compose` t = Map.map (subst s) t `Map.union` s

class Substitutable a where
  subst :: Subst -> a -> a
  freeTVars :: a -> Set.Set String

instance Substitutable Type where
  subst s t = case t of
    TVar a -> Map.findWithDefault t a s
    TApp t ts -> TApp t (map (subst s) ts)
    TArrow t u -> TArrow (subst s t) (subst s u)
    TImplicit n t u ->
      let s' = n `Map.delete` s in
      TImplicit n (subst s' t) (subst s' u)
    TNamed n t ->
      let s' = n `Map.delete` s in
      TNamed n (subst s' t)
    TRec n t ->
      let s' = n `Map.delete` s in
      TRec n (subst s' t)
    TDual t -> TDual $ subst s t
    TOffer s os ->
      let os' = map (\(n, i, r) -> (n, subst s i, subst s r)) os in
      TOffer s os'

  freeTVars t = case t of
    TRec tv t -> tv `Set.delete` freeTVars t
    TImplicit _ t u -> freeTVars t `Set.union` freeTVars u
    TNamed _ t -> freeTVars t
    TVar tv -> Set.singleton tv
    TApp _ ts -> Set.unions $ map freeTVars ts
    TArrow t u -> freeTVars t `Set.union` freeTVars u
    TDual t -> freeTVars t
    TOffer s os -> Set.unions $ map (\(_, i, r) -> freeTVars i `Set.union` freeTVars r) os

instance Substitutable Scheme where -- )FIXME? Constraints treated wrong(?)
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

-- TODO: Unify type-level identifiers?
unify :: Type -> Type -> Infer Subst
unify (TVar a) t = bind a t
unify t (TVar a) = bind a t
unify t u = case (t, u) of
  (TRec tv t, TRec tv', t') -> do
    s1 <- bind tv (TVar tv')
    s2 <- unify (subst s1 t) (subst s1 t')
    return $ s2 `compose` s1
  (TImplicit _ t u, TImplicit _ t' u') -> do
    s1 <- unify t t'
    s2 <- unify (subst s1 u) (subst s1 u')
  (TNamed _ t, TNamed _ t') -> unify t t'
  (TApp a ts, TApp b ts') ->
    if a == b
      then foldr (uncurry compose) nullSubst (zip ts ts')
      else throwError "Unification failed on TApp"
  (TArrow l r, TArrow l' r') -> do
    s1 <- unify l l'
    s2 <- unify (subst s1 r) (subst s1 r')
    return $ s2 `compose` s1
  (TDual t, TDual t') -> unify t t'
  (TOffer _ os, TOffer _ os') -> do
    let unifyOffers [] [] = nullSubst
        unifyOffers (_, i, r):os (_, i', r'):os' = do
          s1 <- unify i i'
          s2 <- unify (subst s1 r) (subst s1 r')
          s3 <- unifyOffers os os'
          return $ s3 `compose` s2 `compose` s1 in
    unifyOffers os os'

instantiate :: Scheme -> Infer Type
instantiate = undefined

generalize :: TypeEnv -> Type -> Scheme
generalize = undefined

class Inferable a where
  infer :: TypeEnv -> a -> Infer (Subst, Type)

-- FIXME: Allow letrec/mutual recursion
instance forall a. Inferable (Expr a) where
  infer env e = case e of
    ELiteral (LBool _)    -> return (nullSubst, tBool)
    ELiteral (LInteger _) -> return (nullSubst, tInteger)
    ELiteral (LDecimal _) -> return (nullSubst, tFloat)
    ELiteral (LString _)  -> return (nullSubst, tString)

    EIdent v -> lookupEnv env x

    ELet id x e -> do
      (s1, t1) <- infer env x
      let env'  = subst s1 env
          t'    = generalize env' t1
      (s2, t2) <- infer (Map.insert id t' env) e
      return (s1 `compose` s2, t2)

    ESession bs -> do
      env <- prepareEnv bs
      let inferBranches [] = fresh >>= \tv -> return (nullSubst, TOffer tv [])
          inferBranches (b:bs) = case b of
            TInit es -> do
              (s1, t1) <- infer env es
              (s2, t2) <- inferBranches (subst s1 bs)
              -- FIXME? I think this (may) require unifying c & t2
              return $ case t1 of
                TBranch x os -> TBranch x $ map (\(l, t, c) -> (l, t, t2)) os
                TChoose x os -> TChoose x $ map (\(l, t, c) -> (l, t, t2)) os
                _ -> t2
            TBranch msg args tgt e ->

    ECond pred t f ->

    ELambda id e ->

    EApply f x ->

    EBranch msg args tgt e ->

    EChoose tgt msg ->

    ESpawn v ->

    ESequence s ->

instance forall a. Inferable (Top a) where
  infer env e = case e of
    TModule m ts -> return $ infer env ts -- FIXME: Handle module scoping?

    TRequire m -> return (nullSubst, tInt) -- FIXME: Import scope?

    TLet id x -> do
      placeHolder <- lookupEnv env id
      (s, t) <- infer env x
      s' <- unify placeHolder (subst s t)
      let env' = subst s' env
          t'  = generalize env' t
      return (s', t')

    TInit e -> infer env e

    TBranch msg args tgt e ->

instance Inferable x => Inferable [x] where
  infer env [] = fresh >>= \tv -> (nullSubst, tv)
  infer env (x:xs) = do
    (s, _) <- infer env x
    (ss, t) <- infer env (subst s xs)
    return (s `compose` ss, t)

prepareEnv :: forall a. [Top a] -> Infer TypeEnv
prepareEnv [] = return Map.empty
prepareEnv (t:ts) = case t of
  TLet id _ -> do
    tyId <- fresh
    return $ Map.insert id tyId (prepareEnv ts)
  _ -> prepareEnv ts -- FIXME: Should imports, etc. be handled here?
