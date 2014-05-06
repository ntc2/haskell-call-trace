-- http://hackage.haskell.org/package/base-4.7.0.0/docs/System-IO-Unsafe.html#v:unsafePerformIO
{-# OPTIONS_GHC -fno-cse -fno-full-laziness #-}

{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE ConstraintKinds #-}
{-# LANGUAGE BangPatterns #-}

module Data.Function.Decorator.Logger.Unsafe where

import Prelude hiding (curry , uncurry)

import Data.IORef
import Data.Proxy
import System.IO.Unsafe

import Data.Function.Decorator.Curry
import Data.Function.Decorator.Logger.HetCall
import Data.Function.Decorator.Logger.Logger

----------------------------------------------------------------

{-# NOINLINE globalIndentLevel #-}
globalIndentLevel :: IORef Int
globalIndentLevel = unsafePerformIO $ newIORef 0

-- This is a special case of a general pattern: make a unsafe version
-- of a safe function by wrapping in 'unsafePeformBlah'.

{-# NOINLINE unsafeTrace #-}
unsafeTrace :: forall n t.
  ( CurryUncurry n t
  , UncurryCurry n (Args n t) (IO (Ret n t))
  , UncurryMCurry  (Args n t)  IO (Ret n t)
  , HFold Show (Args n t)
  , Show (Ret n t)
  ) => Proxy n -> String -> t -> t
unsafeTrace p name f =
  compose p unsafePerformIO' .
  trace (return globalIndentLevel) name $
  compose p return' f
  where
    -- Polymorphism confuses 'compose'.
    return'          :: Ret n t -> IO (Ret n t)
    unsafePerformIO' :: IO (Ret n t) -> Ret n t
    -- Subtle: make our 'return' strict to ensure that effects in
    -- recursive calls wrapped in 'unsafePerformIO' are correctly
    -- sequenced.
    return' !x       = return x
    unsafePerformIO' = unsafePerformIO
