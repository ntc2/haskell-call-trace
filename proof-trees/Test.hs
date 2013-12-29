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
import Control.Monad.Trans.Error
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
  deriving Show
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

type M a = ErrorT String (Writer (LogStream ProofTree)) a

type CheckTy = Tm -> Ty -> M ()

check :: CheckTy
check = undefined
