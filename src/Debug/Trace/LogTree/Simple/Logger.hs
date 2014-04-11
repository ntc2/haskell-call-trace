{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE ConstraintKinds #-}
{-# LANGUAGE UndecidableInstances #-}

module Debug.Trace.LogTree.Simple.Logger where

import Prelude hiding (curry)

import GHC.TypeLits

import Control.Monad.Writer
import Data.Proxy

import Debug.Trace.LogTree
import Debug.Trace.LogTree.Simple.Call
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

-- XXX: this class and instance probably belong somewhere else.
class Monad m => EventLogger c m where
  logEvent :: LogEvent c -> m ()

-- Need 'UndecidableInstances' here!
instance MonadWriter [LogEvent c] m => EventLogger c m where
  logEvent e = tell [e]

-- Note: the 'GetArg t `Curried` GetMonad t (GetRet t)' is just a
-- fancy way to write 't' (that GHC prefers). The synonym
-- 'CurriedUncurriedM t' expands to that.
simpleLogger :: forall tag before t after c
              . ( SingI tag
                , CurryM t
                , UncurryM t
                , EventLogger c (GetMonad t)
                , c (Proxy (SimpleCall tag before t after)) )
             => Proxy (tag::Symbol)
             -> GetMonad t before
             -> GetMonad t after
             -> t
             -> CurriedUncurriedM t
simpleLogger _ ms1 ms2 f = curry k where
  k :: UncurriedM t
  k arg = do
    let call = Proxy::Proxy (SimpleCall tag before t after)
    s1 <- ms1
    logEvent (BeginCall call s1 arg::LogEvent c)
    ret <- uncurryM f arg
    s2 <- ms2
    logEvent (EndCall call s1 arg ret s2::LogEvent c)
    return ret

----------------------------------------------------------------

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

----------------------------------------------------------------
