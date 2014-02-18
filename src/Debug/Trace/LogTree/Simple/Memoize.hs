{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE ConstraintKinds #-}
{-# LANGUAGE UndecidableInstances #-}

module Debug.Trace.LogTree.Simple.Memoize where

import GHC.TypeLits

import Control.Monad.Writer
import Data.Proxy

import Debug.Trace.LogTree
import Debug.Trace.LogTree.Simple.Call
import Debug.Trace.LogTree.Simple.Curry

----------------------------------------------------------------
-- A memoizer with a simple interface.
--
-- Specify the function you want to memoize, along with a 'Symbol' to
-- uniquely tag it (not implemented here), and get back a memoizer for
-- your function.  The pattern for lightweight memoization is then
--
--   f :: t
--   f = e
--
-- becomes
--
--   f , f' :: t
--   f = simpleMemoizer lookup insert f'
--   f' = e
--
-- creating a knot, where recursive calls in 'e' are still to 'f' and
-- hence trigger memoization.  Here 'lookup' and 'insert' are for
-- looking up and caching memo values, respectively.  In a state monad
-- they can update the state; in an IO monad they can update a
-- reference.
--
-- An alternative approach would be
--
--   f , f' :: t
--   f = simpleMemoizer "f" f'
--   f' = e
--
-- where we have a class constraint saying that "f" can be used to
-- look up a memo dictionary. Something like
--
--   class Signature call => Memoizer call m where
--     lookup :: Proxy call -> Arg call -> m (Maybe (Ret call))
--     insert :: Proxy call -> Arg call -> Ret call -> m ()
--
-- and then e.g.
--
--   data S = S { _fDict :: Data.Map (GetArg FTy) (GetRet FTy) , ... }
--
--   instance Memoizer (SimpleCall "f" () FTy ()) (State S) where
--     lookup _ k   = Data.Map.lookup x <$> gets _fDict
--     insert _ k v = modify i where
--       i s = s { _fDict = Data.Map.insert k v $ _fDict s }
--
-- The obvious: with either interface we probably want to use lenses
-- to make defining 'insert' and 'lookup' less painful :P
--
-- In 'IO' this could be pretty nice, if we used 'unsafePerformIO' to
-- get at a global reference:
--
--   f = unsafePerformIO $ ioMemoizer f'
--   f' = e
--
--   ioMemoizer f' = do
--     d <- newIORef Data.Map.empty
--     let lookup k   = Data.Map.lookup k <$> readIORef d
--         insert k v = modifyIORef d (Data.Map.Insert k v)
--     return $ simpleMemoizer lookup insert f' where
--
-- Things get even more interesting with 'Control.Monad.ST':
--
--   - we can define an 'stMemoizer' in the same way as 'ioMemoizer',
--     and then use 'unsafePerformIO . unsafeSTToIO' to escape.
--
--   - we can maybe define an 'stMemoizer' that is pure, but only
--     memoizes the current call. Have to think about this more...
--
-- Also, if go back to standard open recursion, we can get pure
-- per-call versions. E.g.
--
--   f' :: FTy
--   f :: FTy -> FTy
--   f f' = e -- recursive calls to 'f'' in 'e'
--
-- and then e.g. (approximate types)
--
--   simpleMemoizer :: (Arg a -> m (Maybe (Ret a))) ->    -- Insert
--                     (Arg a -> Ret a -> m ()) ->        -- Lookup
--                     (a -> a) ->                        -- Open function to fix
--                     a
--   simpleMemoizer = ...
--
--   ioMemoizer :: (a -> a) -> IO a
--   ioMemoizer f = do
--     d <- newIORef Data.Map.empty
--     return $ simpleMemoizer <lookup for d> <insert for d> f
--
-- The reason we need to return to standard open recursion and fix
-- points here is that we need memoize away from the definition
-- site. Indeed, for the current style with two mutually recursive
-- defs, memoizing at the definition site with
--
--  ioMemoizer :: a -> IO a
--  ioMemoizer = ...
--
--  f , f' :: FTy
--  f = ioMemoizer f'
--
-- is not well typed.
--
-- Another alternative: if we're willing to impose SYB-style
-- constraints on our types, we might be able to use one heterogeneous
-- dictionary to rule them all.  Something like
--
--   Data.Map.Map String (Ex2T Data.Map.Map)
--
-- where we require the existentially quantified inner map type params
-- to be in 'Data' or 'Typeable' (whatever allows us to 'cast').  Then
-- we can look up a dictionary for the current function and cast it to
-- the appropriate type.  The cast is allowed to fail, but it can only
-- fail do to a bug (in other words, we go fully dynamic here ...).
--
-- Now that we only need one dictionary, we require constant
-- boilerplate overhead to memoize 'n' functions (in addition to the
-- 'n' lines saying we want to memoize them :P), and we can even write
-- a default instance for state monad that stores a single dictionary,
-- reducing the boilerplate to one line per function.

-- XXX: this class and instance probably belong somewhere else.
class Monad m => EventLogger c m where
  logEvent :: LogEvent c -> m ()

-- Need 'UndecidableInstances' here!
instance MonadWriter [LogEvent c] m => EventLogger c m where
  logEvent e = tell [e]

-- Note: the 'GetArg t `Curried` GetMonad t (GetRet t)' is just a
-- fancy way to write 't' (that GHC prefers).
simpleLogger :: forall tag before t after c
              . ( SingI tag
                , CollectAndCallCont t
                , EventLogger c (GetMonad t)
                , c (Proxy (SimpleCall tag before t after)) )
             => Proxy (tag::Symbol)
             -> GetMonad t before
             -> GetMonad t after
             -> t
             -> GetArg t `Curried` GetMonad t (GetRet t)
simpleLogger _ ms1 ms2 f = collectAndCallCont k f where
  k :: (GetArg t , GetMonad t (GetRet t)) -> GetMonad t (GetRet t)
  k (arg , mret) = do
    let call = Proxy::Proxy (SimpleCall tag before t after)
    s1 <- ms1
    logEvent (BeginCall call s1 arg::LogEvent c)
    ret <- mret
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
