-- http://hackage.haskell.org/package/base-4.7.0.0/docs/System-IO-Unsafe.html#v:unsafePerformIO
{-# OPTIONS_GHC -fno-cse -fno-full-laziness #-}

{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE ConstraintKinds #-}
{-# LANGUAGE BangPatterns #-}

module Data.Function.Decorator.Memoizer.Unsafe where

import Prelude hiding (lookup)

import Control.Applicative
import Data.IORef
import qualified Data.Map.Strict as Map
import Data.Proxy
import System.IO.Unsafe

import Data.Function.Decorator.Curry
import Data.Function.Decorator.Memoizer
import Data.Function.Decorator.Unsafe

----------------------------------------------------------------

{-# NOINLINE unsafeMemoize #-}
unsafeMemoize ::
  ( UnsafePurifiable n t
  , Ord (Args n t)
  ) => Proxy n -> t -> t
unsafeMemoize p =
  unsafePurify p makeDecorator
  where
    makeDecorator = do
      cacheRef <- newIORef Map.empty
      let lookup arg =
            Map.lookup arg <$> readIORef cacheRef
          insert arg ret =
            modifyIORef' cacheRef (Map.insert arg ret)
      return $ simpleMemoize lookup insert

----------------------------------------------------------------
-- Original version not using 'unsafePurify'.
--
-- Might be easier to understand ...

{-# NOINLINE unsafeMemoize' #-}
unsafeMemoize' :: forall n t.
  ( CurryUncurry n t
  , UncurryCurry n (Args n t) (IO (Ret n t))
  , UncurryMCurry  (Args n t)  IO (Ret n t)
  , Ord (Args n t)
  ) => Proxy n -> t -> t
unsafeMemoize' p f = unsafePerformIO $ do
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
    return'          :: Ret n t -> IO (Ret n t)
    unsafePerformIO' :: IO (Ret n t) -> Ret n t
    -- Subtle: make our 'return' strict to ensure that effects in
    -- recursive calls wrapped in 'unsafePerformIO' are correctly
    -- sequenced.
    return' !x       = return x
    unsafePerformIO' = unsafePerformIO
