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
class Monad (GetMonad t) => UncurryM t where
  type GetArgsM t :: *
  type GetRetM  t :: *
  type GetMonad t :: * -> *
  uncurryM :: t -> UncurriedM t

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
  type GetArgsM (a -> b) = (a , GetArgsM b)
  type GetRetM  (a -> b) = GetRetM b
  type GetMonad (a -> b) = GetMonad b
  uncurryM f (x , xs) = uncurryM (f x) xs

instance (Monad m , Monad (t m)) => UncurryM (t m r) where
  type GetArgsM (t m r) = ()
  type GetRetM  (t m r) = r
  type GetMonad (t m r) = t m
  uncurryM f () = f

instance UncurryM (IO r) where
  type GetArgsM (IO r) = ()
  type GetRetM  (IO r) = r
  type GetMonad (IO r) = IO
  uncurryM f () = f

----------------------------------------------------------------
-- Type level computation of curried types.

class Curry a b where
  type Curried a b :: *
  -- This version is right nested like 'UncurryM.GetArgsM', whereas
  -- iterated 'Prelude.curry' is left nested.  We can probably make a
  -- left-nested version using type-level snoc, but then we may need
  -- type-level snoc lemmas, which will not be fun ...
  curry :: (a -> b) -> a `Curried` b

instance Curry as b => Curry (a , as) b where
  type Curried (a , as) b = a -> Curried as b
  -- The essence of the continuation trick in 'collectAndCallCont'.
  curry f x = curry (\ xs -> f (x , xs))

instance Curry () b where
  type Curried () b = b
  curry f = f ()

type UncurriedM        t = GetArgsM t ->        GetMonad t (GetRetM t)
-- A fancy identity function.
type CurriedUncurriedM t = GetArgsM t `Curried` GetMonad t (GetRetM t)

-- The 'CurryUncurryM' is a constraint synonym and needs
-- '-XConstraintKinds'.
type CurryUncurryM t =
  ( UncurryM t
  , Curry (GetArgsM t) (GetMonad t (GetRetM t))
  , CurriedUncurriedM t ~ t
  )

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

type Uncurried        (n :: Nat) (t :: *) = GetArgs n t -> GetRet n t
type CurriedUncurried (n :: Nat) (t :: *) = GetArgs n t `Curried` GetRet n t
type CurryUncurry     (n :: Nat) (t :: *) =
  ( Uncurry n t
  , Curry (GetArgs n t) (GetRet n t)
  , CurriedUncurried n t ~ t
  )

-- The 'CurryUncurry n t' constraint is not used in the definition, but
-- you want it in practice when you use 'Uncurry n t', and it serves as
-- a sanity check.
class Uncurry (n :: Nat) (t :: *) where
  type GetArgs n t :: *
  type GetRet  n t :: *
  uncurry :: Proxy n -> t -> Uncurried n t

instance Uncurry n b => Uncurry (Succ n) (a -> b) where
  type GetArgs (Succ n) (a -> b) = (a , GetArgs n b)
  type GetRet  (Succ n) (a -> b) = GetRet n b
  uncurry _ f (x , xs) = uncurry (Proxy::Proxy n) (f x) xs

instance Uncurry Zero b where
  type GetArgs Zero b = ()
  type GetRet  Zero b = b
  uncurry _ f () = f

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
compose :: (Uncurry n t , Curry (GetArgs n t) a) =>
  Proxy n -> (GetRet n t -> a) -> t -> GetArgs n t `Curried` a
compose p g f = curry (g . uncurry p f)

----------------------------------------------------------------
