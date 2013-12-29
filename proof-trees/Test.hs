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
import Control.Monad.Error
import Control.Monad.Reader
import Control.Monad.Writer
import Data.List (intercalate)
import Data.Proxy
import Text.Parsec hiding (between)
import qualified Text.Parsec

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
parens = Text.Parsec.between (char '(') (char ')')

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

-- Here 'call*' is the opposite of the standard 'exec*': throw away
-- any non-result state and return the result.
callParser :: P a -> String -> a
callParser p = either (error . show) id
             . parse (p <* eof) "<no file>"

----------------------------------------------------------------
-- Type checker.

type M a = ErrorT String (ReaderT Ctx (Writer (LogStream ProofTree))) a
runM :: M a -> (Either String a , LogStream ProofTree)
runM = runWriter . flip runReaderT [] . runErrorT

ctx :: M Ctx
ctx = ask

type InferTy = Tm -> M Ty
infer , infer' :: InferTy
infer = simpleLogger (Proxy::Proxy "infer") ctx (return ()) infer'

infer' (Lam x t e) = local ((x,t):) . infer $ e
infer' (TmVar x) = maybe err pure . lookup x =<< ctx where
  err = throwError $ "Variable " ++ x ++ " not in context!"
infer' (e :@: e1) = do
  t <- infer e
  t1 <- infer e1
  case t of
    t1' :->: t2 | t1' == t1 -> pure t2
    _ -> throwError $ "Can't apply " ++ show t ++ " to " ++ show t1 ++ "!"

callInfer :: Tm -> Ty
callInfer = either error id . fst . runM . infer

----------------------------------------------------------------
-- Latex pretty printer.

-- Precedence: higher value means tighter binding.
type Prec = Double

between :: Prec -> Prec -> Prec
between x y = (x + y) / 2

lowest , highest , precLam , precApp , precArr :: Prec
highest = 1
lowest = 0
precLam = lowest
precApp = between precLam highest
precArr = lowest

-- Associativity: left, none, or right.
data Assoc = L | N | R deriving Eq

-- Wrap a pretty print when the context dictates.
wrap :: Pretty a => Assoc -> a -> a -> String
wrap side ctx x = if prec x `comp` prec ctx
                  then pp x
                  else wrap . pp $ x
  where
    comp = if side == assoc x || assoc x == N
           then (>=)
           else (>)
    wrap s = "(" ++ s ++ ")"

class Pretty t where
  pp :: t -> String
  prec :: t -> Prec
  prec _ = highest
  assoc :: t -> Assoc
  assoc _ = N

instance Pretty Ty where
  pp (TyVar v) = v
  pp t@(t1 :->: t2) = wrap L t t1 ++ " \\to " ++ wrap R t t2
  prec (_ :->: _) = precArr
  prec _ = highest
  assoc (_ :->: _) = R
  assoc _ = N

instance Pretty Tm where
  pp (TmVar v) = v
  pp (Lam x t e) = "\\lambda " ++ x ++ " : " ++ pp t ++ " . " ++ pp e
  pp e@(e1 :@: e2) = wrap L e e1 ++ " " ++ wrap R e e2
  prec (Lam {}) = precLam
  prec (_ :@: _) = precApp
  prec _ = highest
  assoc (_ :@: _) = L
  assoc _ = N

instance Pretty Ctx where
  pp [] = "."
  pp ctx@(_:_) =
    intercalate "," [ x ++ ":" ++ pp t  | (x,t) <- ctx ]

parseAndInfer :: String -> Ty
parseAndInfer = runInfer . either (error . show) id . parseTm
