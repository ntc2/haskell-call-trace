{-# LANGUAGE DataKinds #-}
{-# LANGUAGE KindSignatures #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE FlexibleContexts #-}

module Data.Function.Decorator.Logger.SimpleCall where

import GHC.TypeLits

import Data.Function.Decorator.Logger.LogTree
import Data.Function.Decorator.Curry

----------------------------------------------------------------
-- A generic call-signature type for a generic logger.

-- A 'SimpleCall' is tagged with a function name so that we can
-- distinguish functions with the same 'before', 'sig', and 'after'
-- types.
data SimpleCall (tag::Symbol) before sig after = SimpleCall
  deriving Show

-- The 'GHC.TypeLits' docs are at
--
-- https://ghc.haskell.org/trac/ghc/wiki/TypeNats/Basics
--
-- but I ran into the problem described here (bug where type error
-- messages print kind parameters):
--
-- http://stackoverflow.com/questions/12569386/cant-use-ghci-inferred-type-signature-for-function-returning-sing-d-symbol
instance (SingI tag , UncurryM sig)
      => Signature (SimpleCall tag before sig after) where
  -- Get the value-level string corresponding to the type-level string
  -- 'tag'.
  name _ = fromSing (sing::Sing tag)
  type Before (SimpleCall tag before sig after) = before
  type Args   (SimpleCall tag before sig after) = ArgsM sig
  type Ret    (SimpleCall tag before sig after) = RetM sig
  type After  (SimpleCall tag before sig after) = after

----------------------------------------------------------------
