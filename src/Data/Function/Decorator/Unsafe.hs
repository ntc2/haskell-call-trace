-- http://hackage.haskell.org/package/base-4.7.0.0/docs/System-IO-Unsafe.html#v:unsafePerformIO
{-# OPTIONS_GHC -fno-cse -fno-full-laziness #-}

{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE ConstraintKinds #-}

module Data.Function.Decorator.Unsafe where

import Prelude hiding (curry , uncurry)

import Control.Exception (evaluate)
import Data.Proxy
import System.IO.Unsafe

import Data.Function.Decorator.Curry

----------------------------------------------------------------
-- Turn a (safe) decorator for monadic functions into a decorator for
-- pure functions.
--
-- The trick is to make a new decorator which applies the original
-- decorator to a pure function by turning the pure function into a
-- monadic function and then using 'unsafePerformIO' to purify the
-- result. The pure function is made monadic by wrapping its result in
-- 'return'.

-- Constraints ensuring all the currying and uncurrying does what
-- you'd expect.
type UnsafePurifiable n t =
  ( CurryUncurry n t
  , UncurryCurry n (Args n t) (IO (Ret n t))
  , UncurryMCurry  (Args n t)  IO (Ret n t)
  )
-- The type 't' with the range type wrapped in 'IO'.
type InjectIO n t = Args n t ->* IO (Ret n t)

{-# NOINLINE unsafePurify #-}
unsafePurify :: forall n t.
  UnsafePurifiable n t =>
  Proxy n -> IO (InjectIO n t -> InjectIO n t) -> t -> t
unsafePurify p makeDecorator = unsafePerformIO $ do
  decorate <- makeDecorator
  return $
    compose p unsafePerformIO' .
    decorate .
    compose p return'
  where
    -- Polymorphism confuses 'compose'.
    return'          :: Ret n t -> IO (Ret n t)
    unsafePerformIO' :: IO (Ret n t) -> Ret n t
    -- Subtle: make our 'return' strict to ensure that effects in
    -- recursive calls wrapped in 'unsafePerformIO' are correctly
    -- sequenced.
    --
    -- Here 'return' !x = return x' also works.
    return'          = evaluate
    unsafePerformIO' = unsafePerformIO
