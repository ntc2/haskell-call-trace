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
-- XXX: introduce a synonym for this long type:
--
--  , GetArgs n t `Curried` IO (GetRet n t) ~ foo
--
-- or even better: introduce one big synonym for all of these
-- constraints!
  , GetArgs n (GetArgs n t `Curried` IO (GetRet n t)) ~ GetArgs n t
  , GetRet  n (GetArgs n t `Curried` IO (GetRet n t)) ~ IO (GetRet n t)

  , GetArgsM  (GetArgs n t `Curried` IO (GetRet n t)) ~ GetArgs n t
  , GetRetM   (GetArgs n t `Curried` IO (GetRet n t)) ~ GetRet n t
  , GetMonad  (GetArgs n t `Curried` IO (GetRet n t)) ~ IO

  , Curry (GetArgs n t) (IO (GetRet n t))
  , Uncurry n (GetArgs n t `Curried` IO (GetRet n t))
  , UncurryM  (GetArgs n t `Curried` IO (GetRet n t))

  , HFold Show (GetArgs n t)
  , Show (GetRet n t)
  ) => Proxy n -> String -> t -> t
unsafeTrace p name f =
  compose p unsafePerformIO' .
  trace (return globalIndentLevel) name $
  compose p return' f
  where
    -- Polymorphism confuses 'compose'.
    return'          :: GetRet n t -> IO (GetRet n t)
    unsafePerformIO' :: IO (GetRet n t) -> GetRet n t
    -- Subtle: make our 'return' strict to ensure that effects in
    -- recursive calls wrapped in 'unsafePerformIO' are correctly
    -- sequenced.
    return' !x       = return x
    unsafePerformIO' = unsafePerformIO
