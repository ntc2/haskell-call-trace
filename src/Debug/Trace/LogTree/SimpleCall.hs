{-# LANGUAGE DataKinds
           , KindSignatures
           , TypeFamilies
           , ScopedTypeVariables
           , FlexibleInstances
           , FlexibleContexts
           #-}

module Debug.Trace.LogTree.SimpleCall where

import GHC.TypeLits

import Control.Monad.Identity

import Debug.Trace.LogTree

----------------------------------------------------------------
-- Type-level uncurry for type signatures that end in a monad.

-- Tricky: to put a class constraint on an associated type we make it
-- a premise in the class signature!  I can't find any way to put it
-- in the class body directly.
class Monad (GetMonad t) => UncurryM t where
  type GetArg t
  type GetRet t
  type GetMonad t

-- It seems I've got this working for all types of the form
--
--   a1 -> ... -> an -> m a
--
-- where 'm' is the identity monad, or any monad with a transformer at
-- the outside.  I expect this covers all standard non-arrow
-- monads!
--
-- How it works: the naive problem is that '((->) r) :: * -> *' has
-- the same kind as a monad -- and has is a standard monad instance,
-- but that's beside the point, since the open-world assumption for
-- type classes means if the types unify then they overlap -- and so
-- it's difficult to write a base case that just looks for monads.
-- However, by using the constraints
--
--   (Monad m , Monad (t m)) => UncurryM (t m r)
--
-- we force 't' to have kind '(* -> *) -> *', which '(->)' doesn't
-- match.  Hence we avoid overlap.
--
-- This is nice because a little googling showed it was probably not
-- easy to do this more generally, i.e. when the base type is not a
-- monad application people run into trouble, and have to declare many
-- base type instances and use undecidable instances. For instance:
--
-- http://ro-che.info/articles/2013-01-29-generic-uncurry.html
-- http://www.haskell.org/haskellwiki/GHC/AdvancedOverlap
--
-- If for some reason I want to use a custom monad which is not a
-- transformer and which is not transformed, then I'll need another
-- instance, but that case seems unlikely.
instance UncurryM b => UncurryM (a -> b) where
  type GetArg (a -> b) = (a , GetArg b)
  type GetRet (a -> b) = GetRet b
  type GetMonad (a -> b) = GetMonad b

instance UncurryM (Identity a) where
  type GetArg (Identity a) = ()
  type GetRet (Identity a) = a
  type GetMonad (Identity a) = Identity

instance (Monad m , Monad (t m)) => UncurryM (t m r) where
  type GetArg (t m r) = ()
  type GetRet (t m r) = r
  type GetMonad (t m r) = (t m)

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
