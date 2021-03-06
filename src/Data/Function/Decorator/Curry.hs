{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE ConstraintKinds #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE DataKinds #-}

module Data.Function.Decorator.Curry where

import Prelude hiding (curry , uncurry)

import Data.Proxy
import Language.Haskell.TH

----------------------------------------------------------------
-- Type-level uncurry for type signatures that end in a monad.

-- Tricky: to put a class constraint on an associated type we make it
-- a premise in the class signature!  I can't find any way to put it
-- in the class body directly.
class Monad (MonadM t) => UncurryM (t :: *) where
  type ArgsM  t :: *
  type RetM   t :: *
  type MonadM t :: * -> *
  uncurryM :: t -> UncurriedM t

type UncurriedM t = ArgsM t -> MonadM t (RetM t)

-- It seems I've got this working for all types of the form
--
--   a1 -> ... -> an -> m a
--
-- where 'm' is 'IO', or any monad with a transformer at the outside.
-- I expect this covers all standard non-arrow monads I'd want to use!
--
-- How it works: the naive problem is that '((->) r) :: * -> *' has
-- the same kind as a monad -- and has a standard monad instance,
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
-- instance, but that case seems unlikely. If it does happen, it's as
-- simple as copying and adapting the 't m r' instances.
instance UncurryM b => UncurryM (a -> b) where
  type ArgsM  (a -> b) = (a , ArgsM b)
  type RetM   (a -> b) = RetM b
  type MonadM (a -> b) = MonadM b
  uncurryM f (x , xs) = uncurryM (f x) xs

instance (Monad m , Monad (t m)) => UncurryM (t m r) where
  type ArgsM  (t m r) = ()
  type RetM   (t m r) = r
  type MonadM (t m r) = t m
  uncurryM f () = f

instance UncurryM (IO r) where
  type ArgsM  (IO r) = ()
  type RetM   (IO r) = r
  type MonadM (IO r) = IO
  uncurryM f () = f

----------------------------------------------------------------
-- Type level computation of curried types.

class Curry (as :: *) (b :: *) where
  -- Compute the curried type corresponding to the uncurried type with
  -- domain tuple 'as' and range 'bs'.
  --
  -- Mnemonic: 'as ->* b' means "insert zero or more ('*'-many) arrows
  -- between the 'as' and 'b'.
  type as ->* b :: *
  -- This version is right nested like 'UncurryM.ArgsM', whereas
  -- iterated 'Prelude.curry' is left nested.  We can probably make a
  -- left-nested version using type-level snoc, but then we may need
  -- type-level snoc lemmas, which will not be fun ...
  curry :: (as -> b) -> as ->* b

instance Curry as b => Curry (a , as) b where
  type (a , as) ->* b = a -> as ->* b
  -- The essence of the continuation trick in 'collectAndCallCont'.
  curry f x = curry (\ xs -> f (x , xs))

instance Curry () b where
  type () ->* b = b
  curry f = f ()

----------------------------------------------------------------
-- Uncurrying which does not assume a monadic return.
--
-- In GHC 7.8 we can compute the minimal return type using ordered
-- overlapping instances in a closed type function, but it's not
-- actually clear that this is what a user wants in general: e.g., the
-- type
--
--   a -> b -> c
--
-- could get uncurried as '(a , (b , ())) -> c', with "minimal return
-- type" 'c', or as '(a , ()) -> (b -> c)', with return type 'b -> c'.
-- So, non-monadic uncurrying is ambiguous in general!

-- The 'CurryUncurry n t' constraint is not used in the definition, but
-- you want it in practice when you use 'Uncurry n t', and it serves as
-- a sanity check.
class Uncurry (n :: Nat) (t :: *) where
  type Args n t :: *
  type Ret  n t :: *
  uncurry :: Proxy n -> t -> Uncurried n t

type Uncurried n t = Args n t -> Ret n t

instance Uncurry n b => Uncurry (Succ n) (a -> b) where
  type Args (Succ n) (a -> b) = (a , Args n b)
  type Ret  (Succ n) (a -> b) = Ret n b
  uncurry _ f (x , xs) = uncurry (Proxy::Proxy n) (f x) xs

instance Uncurry Zero b where
  type Args Zero b = ()
  type Ret  Zero b = b
  uncurry _ f () = f

----------------------------------------------------------------
-- Type-level nats for specifying how many args to uncurry.

-- See
-- http://stackoverflow.com/questions/20809998/type-level-nats-with-literals-and-an-injective-successor-n-ary-compose
-- for why we don't use 'GHC.TypeLits.Nat'.
data Nat = Zero | Succ Nat

-- Type-level nat literals (almost).
--
-- Write '$(nat n)' for a literal 'n'.
nat :: Integer -> Q Type
nat 0 = [t| Zero |]
nat n = [t| Succ $(nat (n-1)) |]

proxyNat :: Integer -> Q Exp
proxyNat n = [| Proxy :: Proxy $(nat n) |]

----------------------------------------------------------------
-- An 'n'-ary compose.

-- The type looks fancy, but e.g. if
--
--   return :: b -> m b
--   f      :: a1 -> a2 -> b
--
-- then
--
--   compose (Proxy::Proxy $(nat 2)) return f :: a1 -> a2 -> m b
compose :: (Uncurry n t , Curry (Args n t) a) =>
  Proxy n -> (Ret n t -> a) -> t -> Args n t ->* a
compose p g f = curry (g . uncurry p f)

----------------------------------------------------------------
-- Laws relating currying and uncurrying.
--
-- We can't prove these laws in general (universally quantified), but
-- in any specific case (instantiation) GHC can check that they hold.
--
-- The laws are constraint synonyms and need '-XConstraintKinds'.

----------------------------------------------------------------

-- Laws for currying *after* uncurrying.

type CurryUncurryM (t :: *) =
  ( UncurryM t
  , Curry (ArgsM t) (MonadM t (RetM t))
  , (ArgsM t ->* MonadM t (RetM t)) ~ t
  )

type CurryUncurry (n :: Nat) (t :: *) =
  ( Uncurry n t
  , Curry (Args n t) (Ret n t)
  , (Args n t ->* Ret n t) ~ t
  )

----------------------------------------------------------------

-- Laws for currying *before* uncurrying.

type UncurryCurry (n :: Nat) (as :: *) (r :: *) =
  ( Curry as r
  , Uncurry n (as ->* r)
  , Args n (as ->* r) ~ as
  , Ret  n (as ->* r) ~ r
  )

type UncurryMCurry (as :: *) (m :: * -> *) (r :: *) =
  ( Curry as (m r)
  , UncurryM (as ->* m r)
  , ArgsM  (as ->* m r) ~ as
  , RetM   (as ->* m r) ~ r
  , MonadM (as ->* m r) ~ m
  )
