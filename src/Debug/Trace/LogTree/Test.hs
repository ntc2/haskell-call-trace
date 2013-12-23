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

module Debug.Trace.LogTree.Test where

import GHC.Prim (Constraint)

import Control.Monad.Trans.Maybe
import Control.Monad.Writer
import Data.Proxy
import Text.Parsec

import Debug.Trace.LogTree
import Debug.Trace.LogTree.Simple.Logger

----------------------------------------------------------------
-- Unused constraint logic stuff.

-- XXX: I can't partially apply a synonym:
{-
type ShowWith c t = (Show t , c t)
-}
-- Did I work around this in a similar example when I was doing the
-- het lists for Max? The point is that I'd like a 'Show' instance
-- that works for 'LogTree c' with any constraint 'c' that implies
-- 'Show'.
--
-- Making a some special classes kind of works, e.g.:
class (c1 t , c2 t) => (c1 :&&: c2) t
instance (c1 t , c2 t) => (c1 :&&: c2) t

class Unit t
instance Unit t
-- but this doesn't really give implication. We can almost get
-- implication for constraints built from the combinators with
class (c1 :: * -> Constraint) :=>: (c2 :: * -> Constraint)
instance c :=>: c
instance (c1 :=>: c) => (c1 :&&: c2) :=>: c
{-
instance (c2 :=>: c) => (c1 :&&: c2) :=>: c
-}
-- except the last instance overlaps with the previous one, so we
-- might need something like instance chains here.

----------------------------------------------------------------

deriving instance Show (LogTree AllShow)
deriving instance Show (LogEvent AllShow)

instance Signature String where
  name = id
  type Before String = String
  type Arg String = String
  type Ret String = String
  type After String = String

----------------------------------------------------------------
-- Manual logging example.

type FTy = Int -> MaybeT (Writer (LogStream AllShow)) String

fManual :: FTy
f' :: FTy -> FTy
fManual n = do
  tell [BeginCall "f" "" (show n)]
  r <- f' fManual n
  tell [EndCall "f" "" (show n) r ""]
  return r
f' _ 0 = return ""
f' f 5 = do
  _ <- f 2
  fail ""
f' f n = do
  r <- f (n - 1)
  return $ "X" ++ r

----------------------------------------------------------------
-- Auto logging example.

class (Signature t , Show t , Show (Before t) , Show (Arg t) , Show (Ret t) , Show (After t)) => AllShow t
instance (Signature t , Show t , Show (Before t) , Show (Arg t) , Show (Ret t) , Show (After t)) => AllShow t

fSimple :: FTy
fSimple = simpleLogger (Proxy::Proxy "g") (return ()) (return ()) (f' fSimple)

----------------------------------------------------------------

testStream :: FTy -> Int -> LogStream AllShow
testStream f = execWriter . runMaybeT . f

testForest :: FTy -> Int -> Either ParseError (LogForest AllShow)
testForest f = stream2Forest . testStream f

----------------------------------------------------------------

main :: IO ()
main = do
  forM_ [fManual , fSimple] $ \f -> do
    print $ testStream f 4
    print $ testForest f 4
    putStrLn ""
    print $ testStream f 6
    print $ testForest f 6
    putStrLn ""
    putStrLn ""

----------------------------------------------------------------
