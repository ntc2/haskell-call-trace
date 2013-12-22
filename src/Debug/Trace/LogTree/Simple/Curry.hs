{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE ScopedTypeVariables #-}

module Debug.Trace.LogTree.Simple.Curry where

import Control.Monad.Identity

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
-- where 'm' is any monad with a transformer at the outside.  I expect
-- this covers all standard non-arrow monads I'd want to use!
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
  type GetArg (a -> b) = (a , GetArg b)
  type GetRet (a -> b) = GetRet b
  type GetMonad (a -> b) = GetMonad b

instance (Monad m , Monad (t m)) => UncurryM (t m r) where
  type GetArg (t m r) = ()
  type GetRet (t m r) = r
  type GetMonad (t m r) = (t m)

----------------------------------------------------------------
-- Type level computation of curried types.

class Curry a b where
  type Curried a b

instance Curry () b where
  type Curried () b = b

instance Curry as b => Curry (a , as) b where
  type Curried (a , as) b = a -> Curried as b

----------------------------------------------------------------
-- Collection of curried (tupled) arguments while calling.

-- XXX: The 'tagged' package on hackage provides 'Proxy' and 'Tagged'
-- types with some operations.
data Proxy t = Proxy

class (UncurryM b , UncurryM c) => Collect b c where
  collectCallHelper :: Proxy c
                    -> (GetArg b -> GetArg c)
                    -> b
                    -> Curried (GetArg b)
                               (GetArg c , GetMonad b (GetRet b))

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
