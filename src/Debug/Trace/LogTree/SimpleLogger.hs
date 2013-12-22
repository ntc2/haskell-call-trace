{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE ScopedTypeVariables #-}

module Debug.Trace.LogTree.SimpleLogger where

import Control.Monad.Identity

import Debug.Trace.LogTree.Simple.Curry

----------------------------------------------------------------
-- An event logger with a simple interface.
--
-- Specify the function you want to trace, along with a 'Symbol' to
-- uniquely tag it (not implemented here), and get back a tracer for
-- your function.  The pattern for lightweight logging is then
--
--   f :: t
--   f = e
--
-- becomes
--
--   f , f' :: t
--   f = simpleLogger "f" f'
--   f' = e
--
-- creating a knot, where recursive calls in 'e' are still to 'f' and
-- hence trigger recursive logging.

-- XXX: The 'tagged' package on hackage provides 'Proxy' and 'Tagged'
-- types with some operations.
data Proxy t = Proxy

logB :: UncurryM t => Proxy t -> GetArg t -> GetMonad t ()
logB = undefined

logE :: UncurryM t => Proxy t -> GetRet t -> GetMonad t ()
logE = undefined

simpleLogger :: forall t. Collect t t => t -> t
simpleLogger = simpleLoggerHelper (Proxy::Proxy t) id

class (UncurryM b , UncurryM t) => Collect b t where
  simpleLoggerHelper :: Proxy t -> (GetArg b -> GetArg t) -> b -> b

-- BUG?: GHC doesn't like it when I lift a particular set of
-- constraints and give them a name:
{-
{-# LANGUAGE ConstraintKinds #-}

type Suffix b t = (UncurryM b , UncurryM t , GetRet b ~ GetRet t , GetMonad b ~ GetMonad t)

instance Suffix (Identity r) t => Collect (Identity r) t where
  simpleLoggerHelper p acc idr = do
    logB p (acc ())
    r <- idr
    logE p r
    return r
-}
-- generates:
{-
Debug/Trace/LogTree/SimpleLogger.hs:55:10:
    Variable s `r, t, r, t' occur more often than in the instance head
      in the constraint: Suffix (Identity r) t
    (Use -XUndecidableInstances to permit this)
    In the instance declaration for `Collect (Identity r) t'
Failed, modules loaded: Debug.Trace.LogTree, Debug.Trace.LogTree.SimpleCall.
-}
-- whereas the expanded versions below are fine.  Of course, the
-- versions below are only "smaller" because the definitions of the
-- type functions 'GetRet' and 'GetMonad' have been expanded.

instance (UncurryM t , GetRet t ~ r , GetMonad t ~ Identity) => Collect (Identity r) t where
  simpleLoggerHelper p acc idr = do
    logB p (acc ())
    r <- idr
    logE p r
    return r

instance (UncurryM t , GetRet t ~ r , GetMonad t ~ (trans m) , Monad (trans m) , Monad m)
      => Collect (trans m r) t where
  simpleLoggerHelper p acc tmr = do
    logB p (acc ())
    r <- tmr
    logE p r
    return r

instance (UncurryM t , UncurryM b , Collect b t , GetRet t ~ GetRet b , GetMonad t ~ GetMonad b)
      => Collect (a -> b) t where
  simpleLoggerHelper p acc f x =
    simpleLoggerHelper p (\xs -> acc (x , xs)) (f x)

----------------------------------------------------------------
