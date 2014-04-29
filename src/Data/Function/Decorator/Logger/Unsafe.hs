-- http://hackage.haskell.org/package/base-4.7.0.0/docs/System-IO-Unsafe.html#v:unsafePerformIO
{-# OPTIONS_GHC -fno-cse -fno-full-laziness #-}

{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE ConstraintKinds #-}

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
--  , GetArg n t `Curried` IO (GetRet n t) ~ foo
--
-- or even better: introduce one big synonym for all of these
-- constraints!
  , GetArg n (GetArg n t `Curried` IO (GetRet n t)) ~ GetArg n t
  , GetRet n (GetArg n t `Curried` IO (GetRet n t)) ~ IO (GetRet n t)

  , GetArgM   (GetArg n t `Curried` IO (GetRet n t)) ~ GetArg n t
  , GetRetM   (GetArg n t `Curried` IO (GetRet n t)) ~ GetRet n t
  , GetMonad  (GetArg n t `Curried` IO (GetRet n t)) ~ IO

  , Curry (GetArg n t) (IO (GetRet n t))
  , Uncurry n (GetArg n t `Curried` IO (GetRet n t))
  , UncurryM  (GetArg n t `Curried` IO (GetRet n t))

  , HFold Show (GetArg n t)
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
    return'          = return
    unsafePerformIO' = unsafePerformIO
