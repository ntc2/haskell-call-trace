-- Log STLC type checking and process as natural deduction proof tree.

-- See
-- http://stackoverflow.com/questions/3079537/orphaned-instances-in-haskell
{-# OPTIONS_GHC -fno-warn-orphans #-}

{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE ConstraintKinds #-}
{-# LANGUAGE UndecidableInstances #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE GADTs #-}

module Test where

import Control.Applicative ((<*) , (*>) , (<$>) , (<*>) , pure)
-- import Control.Monad.Trans.Error
import Control.Monad.Error
-- import Control.Monad.Trans.Reader
import Control.Monad.Reader
import Control.Monad.Writer
import Data.Proxy
import Text.Parsec

import Debug.Trace.LogTree
import Debug.Trace.LogTree.Process.ProofTree
import Debug.Trace.LogTree.Simple.Logger
import Debug.Trace.LogTree.Simple.Call

----------------------------------------------------------------
-- Grammar.

type TmVar = String
type TyVar = String
data Tm = Lam TmVar Ty Tm
        | TmVar TmVar
        | Tm :@: Tm
  deriving Show
data Ty = TyVar TyVar
        | Ty :->: Ty
  deriving (Eq , Show)
type Ctx = [(TmVar,Ty)]

----------------------------------------------------------------
-- Parser.

type P a = Parsec String () a

parens :: P a -> P a
parens = between (char '(') (char ')')

tmVar , tyVar :: P String
tmVar = (:[]) <$> lower
tyVar = (:[]) <$> upper

tyAtom , arrs , ty :: P Ty
tyAtom = parens ty
     <|> TyVar <$> tyVar
arrs = chainr1 tyAtom arrOp where
  arrOp = string "->" *> pure (:->:)
ty = arrs

tmAtom , apps , lam , tm :: P Tm
tmAtom = parens tm
     <|> TmVar <$> tmVar
apps = chainl1 tmAtom appOp where
  appOp = pure (:@:)
lam = Lam <$> (char '\\' *> tmVar)
          <*> (char ':' *> ty)
          <*> (char '.' *> tm)
tm = apps <|> lam

parseTm :: String -> Either ParseError Tm
parseTm = parse (tm <* eof) "<no file>"

----------------------------------------------------------------
-- Type checker.

type M a = ErrorT String (ReaderT Ctx (Writer (LogStream ProofTree))) a
runM :: M a -> (Either String a , LogStream ProofTree)
runM = runWriter . flip runReaderT [] . runErrorT

type InferTy = Tm -> M Ty
infer :: InferTy
infer (Lam x t e) = local ((x,t):) . infer $ e
infer (TmVar x) = maybe err pure . lookup x =<< ask where
  err = throwError $ "Variable " ++ x ++ " not in context!"
infer (e :@: e1) = do
  t <- infer e
  t1 <- infer e1
  case t of
    t1' :->: t2 | t1' == t1 -> pure t2
    _ -> throwError $ "Can't apply " ++ show t ++ " to " ++ show t1 ++ "!"

runInfer :: Tm -> Ty
runInfer = either error id . fst . runM . infer

----------------------------------------------------------------
-- Utilities.

parseAndInfer :: String -> Ty
parseAndInfer = runInfer . either (error . show) id . parseTm
