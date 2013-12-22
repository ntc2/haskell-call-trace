{-# LANGUAGE DataKinds #-}
{-# LANGUAGE KindSignatures #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE FlexibleContexts #-}

module Debug.Trace.LogTree.SimpleCall where

import GHC.TypeLits

import Debug.Trace.LogTree

import Debug.Trace.LogTree.Simple.Curry

----------------------------------------------------------------
-- A generic call-signature type for a generic logger.

data SimpleCall (tag::Symbol) before sig after where

-- The 'GHC.TypeLits' docs are at
--
-- https://ghc.haskell.org/trac/ghc/wiki/TypeNats/Basics
--
-- but I ran into the problem described here (bug where type error
-- messages print kind parameters):
--
-- http://stackoverflow.com/questions/12569386/cant-use-ghci-inferred-type-signature-for-function-returning-sing-d-symbol
instance (SingI tag , UncurryM sig) => Signature (SimpleCall tag before sig after) where
  -- Get the value-level string corresponding to the type-level string
  -- 'tag'.
  name _ = fromSing (sing::Sing tag)
  type Arg (SimpleCall tag before sig after) = GetArg sig
  type Ret (SimpleCall tag before sig after) = GetRet sig

----------------------------------------------------------------
