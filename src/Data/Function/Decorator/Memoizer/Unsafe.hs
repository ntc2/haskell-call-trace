-- http://hackage.haskell.org/package/base-4.7.0.0/docs/System-IO-Unsafe.html#v:unsafePerformIO
{-# OPTIONS_GHC -fno-cse -fno-full-laziness #-}

{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE ConstraintKinds #-}
{-# LANGUAGE BangPatterns #-}

module Data.Function.Decorator.Memoizer.Unsafe where

import Prelude hiding (curry , uncurry)

import Control.Applicative
import Data.IORef
import qualified Data.Map.Strict as Map
import Data.Proxy
import System.IO.Unsafe

import Data.Function.Decorator.Curry
-- import Data.Function.Decorator.Logger.HetCall
import Data.Function.Decorator.Memoizer

----------------------------------------------------------------

{-# NOINLINE globalIndentLevel #-}
globalIndentLevel :: IORef Int
globalIndentLevel = unsafePerformIO $ newIORef 0

-- This is a special case of a general pattern: make a unsafe version
-- of a safe function by wrapping in 'unsafePeformBlah'.

{-# NOINLINE unsafeMemoize #-}
unsafeMemoize :: forall n t.
  ( CurryUncurry n t

  , Ord      (GetArgs n t)

  , GetArgs n (GetArgs n t `Curried` IO (GetRet n t)) ~ GetArgs n t
  , GetRet  n (GetArgs n t `Curried` IO (GetRet n t)) ~ IO (GetRet n t)

  , GetArgsM  (GetArgs n t `Curried` IO (GetRet n t)) ~ GetArgs n t
  , GetRetM   (GetArgs n t `Curried` IO (GetRet n t)) ~ GetRet n t
  , GetMonad  (GetArgs n t `Curried` IO (GetRet n t)) ~ IO

  , Curry (GetArgs n t) (IO (GetRet n t))
  , Uncurry n (GetArgs n t `Curried` IO (GetRet n t))
  , UncurryM  (GetArgs n t `Curried` IO (GetRet n t))
  ) => Proxy n -> t -> t
unsafeMemoize p f = unsafePerformIO $ do
  cacheRef <- newIORef Map.empty
  let lookup arg =
        Map.lookup arg <$> readIORef cacheRef
      insert arg ret =
        modifyIORef' cacheRef (Map.insert arg ret)

  return $
    compose p unsafePerformIO' .
    simpleMemoize lookup insert $
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
