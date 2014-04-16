{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE ConstraintKinds #-}

module Debug.Trace.LogTree.Simple.Curry where

import Prelude hiding (curry)

import Data.Proxy

----------------------------------------------------------------
-- Type-level uncurry for type signatures that end in a monad.

-- Tricky: to put a class constraint on an associated type we make it
-- a premise in the class signature!  I can't find any way to put it
-- in the class body directly.
class Monad (GetMonad t) => UncurryM t where
  type GetArg   t :: *
  type GetRet   t :: *
  type GetMonad t :: * -> *
  uncurryM :: t -> GetArg t -> GetMonad t (GetRet t)

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
  type GetArg   (a -> b) = (a , GetArg b)
  type GetRet   (a -> b) = GetRet b
  type GetMonad (a -> b) = GetMonad b
  uncurryM f (x , xs) = uncurryM (f x) xs

instance (Monad m , Monad (t m)) => UncurryM (t m r) where
  type GetArg   (t m r) = ()
  type GetRet   (t m r) = r
  type GetMonad (t m r) = t m
  uncurryM f () = f

instance UncurryM (IO r) where
  type GetArg   (IO r) = ()
  type GetRet   (IO r) = r
  type GetMonad (IO r) = IO
  uncurryM f () = f

----------------------------------------------------------------
-- Type level computation of curried types.

class Curry a b where
  type Curried a b :: *
  -- This version is right nested like 'UncurryM.GetArg', whereas
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

-- The 'CurryM' and 'CurryUncurryM' are constraint synonyms and need
-- '-XConstraintKinds'.
type CurryM t = Curry (GetArg t) (GetMonad t (GetRet t))
-- type CurryUncurryM t = (CurryM t , UncurryM t)

type UncurriedM t        = GetArg t ->        GetMonad t (GetRet t)
-- A fancy identity function.
type CurriedUncurriedM t = GetArg t `Curried` GetMonad t (GetRet t)


----------------------------------------------------------------
-- Collection of curried (tupled) arguments while calling.

class (UncurryM b , UncurryM c) => Collect b c where
  collectCallHelper :: Proxy c
                    -> (GetArg b -> GetArg c)
                    -> b
                    -> Curried (GetArg b)
                               (GetArg c , GetMonad b (GetRet b))

instance UncurryM c
      => Collect (IO r) c where
  collectCallHelper _ acc tmr = (acc () , tmr)

instance (UncurryM c , Monad m , Monad (t m))
      => Collect (t m r) c where
  collectCallHelper _ acc tmr = (acc () , tmr)

instance (UncurryM b , UncurryM c , Collect b c)
      => Collect (a -> b) c where
  collectCallHelper p acc f x =
    collectCallHelper p (\xs -> acc (x , xs)) (f x)

collectCall :: forall t. Collect t t
            => t -> Curried (GetArg t)
                            (GetArg t , GetMonad t (GetRet t))
collectCall = collectCallHelper (Proxy::Proxy t) id

----------------------------------------------------------------
-- Collect arguments and result and then call continuation.

class UncurryM c => CollectAndCallCont c where
  collectAndCallCont ::
    ((GetArg c , GetMonad c (GetRet c)) -> d) ->
    c -> GetArg c `Curried` d

instance CollectAndCallCont (IO r) where
  collectAndCallCont k tmr = k (() , tmr)

instance (Monad m , Monad (t m)) => CollectAndCallCont (t m r) where
  collectAndCallCont k tmr = k (() , tmr)

instance CollectAndCallCont b => CollectAndCallCont (a -> b) where
  collectAndCallCont k f x =
    collectAndCallCont (\(xs , tmr) -> k ((x , xs) , tmr)) (f x)

----------------------------------------------------------------
