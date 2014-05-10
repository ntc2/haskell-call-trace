-- Simple decorator examples, to compare with the simple Python
-- examples. Each version has a "dead simple" version, which is
-- closest to the Python version but is specialized to 'IO', and a
-- more general version, which although still simple, can be
-- instantiated at more monads ('State' and 'ST', in addition to
-- 'IO').

-- Simple Python decorator examples.
--
-- Tracing:
{-
LEVEL = 0
def trace(f):
  def traced(*args):
    global LEVEL
    print '| '*LEVEL + \
          '%s%s'%(f.__name__ , args)
    LEVEL += 1
    r = f(*args)
    LEVEL -= 1
    print '| '*LEVEL + '%s'%r
    return r
  return traced
-}
-- Memoization:
{-
def memoize(f):
  cache = dict()
  def memoized(*args):
    if args not in cache:
      cache[args] = f(*args)
    return cache[args]
  memoized.__name__ = f.__name__
  return memoized
-}

{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE ConstraintKinds #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE DataKinds #-}

module Main where

import GHC.TypeLits

import Control.Applicative
import Data.Function (fix) -- ???!!! GHCi also fails to find 'fix'. There be dragons ...
import Data.IORef
import qualified Data.Map as Map
import Data.Proxy
import System.IO.Unsafe

import Data.Function.Decorator.Curry as Curry
import Data.Function.Decorator.Logger.HetCall
import Data.Function.Decorator.Unsafe

----------------------------------------------------------------
-- Simple monadic tracers.

-- Dead simple tracer specific to 'IO'.
--
-- This can't be used at the top-level in a safe way, since there's no
-- pure way to get your hands on an 'IORef'.  However, it can be used
-- purely with open definitions.

trace :: forall t.
  ( CurryUncurryM t
  , Show (ArgsM t)
  , Show (RetM t)
  , MonadM t ~ IO ) =>
  IORef Int -> String -> t -> t
trace levelRef name f = Curry.curry k where
  k :: UncurriedM t
  k args = do
    level <- readIORef levelRef
    let prefix = concat . replicate level $ "| "
    putStrLn $ prefix ++ name ++ show args

    modifyIORef levelRef (+1)
    r <- uncurryM f args
    modifyIORef levelRef (subtract 1)

    putStrLn $ prefix ++ show r
    return r

-- More general version that can be instantiated at 'ST', 'State', or
-- 'IO'.  With a slightly different interface, replacing 'incLevel'
-- with 'withIncdLevel :: m a -> m a', we could additionally
-- instantiate at 'Reader', with 'withIncdLevel = local (+1)' and
-- 'getLevel = ask'.

trace' :: forall t m.
  ( CurryUncurryM t
  , HFold Show (ArgsM t)
  , Show (RetM t)
  , MonadM t ~ m ) =>
  m Int -> (Int -> m ()) -> (String -> m ()) ->
  String -> t -> t
trace' getLevel incLevel print name f = Curry.curry k where
  k :: UncurriedM t
  k args = do
    level <- getLevel
    let prefix = concat . replicate level $ "| "
    -- Or: name ++ show args
    print $ prefix ++ formatCall name args

    incLevel 1
    r <- uncurryM f args
    incLevel (-1)

    print $ prefix ++ show r
    return r

----------------------------------------------------------------
-- Simple monadic memoizers.

-- Dead simple memoizer.
--
-- Unlike the dead simple tracer, this can be safely used at the top
-- level, if we create a new cache on each call. E.g.
powM :: Int -> Int -> IO Int
powM b p = do
  cacheRef <- newIORef Map.empty
  fix (trace levelRef "powM" . memoize cacheRef . openPowM) b p
  where
  openPowM pow b p = do
    if p <= 1
    then pure $ b * p
    else (*) <$> pow b (p `div` 2) <*> pow b (p - (p `div` 2))
-- Not bad!
memoize :: forall t.
  ( CurryUncurryM t
  , Ord (ArgsM t)
  , MonadM t ~ IO ) =>
  IORef (Map.Map (ArgsM t) (RetM t)) -> t -> t
memoize cacheRef f = Curry.curry k where
  k :: UncurriedM t
  k args = do
    cache <- readIORef cacheRef
    case Map.lookup args cache of
      Just r  -> return r
      Nothing -> do
        r <- uncurryM f args
        modifyIORef cacheRef (Map.insert args r)
        return r

-- More general but still simple memoizer.
--
-- This one can be instantiated at 'ST' and 'State', as well as
-- 'IO'. I don't think it makes sense in 'Reader', since we need the
-- dictionary deltas from recursive calls to flow back up.
memoize' :: forall t as m r.
  ( CurryUncurryM t
  , Ord (ArgsM t)
  , ArgsM t ~ as
  , MonadM t ~ m
  , RetM t ~ r ) =>
  (as -> m (Maybe r)) -> (as -> r -> m ()) ->
  t -> t
memoize' lookup insert f = Curry.curry k where
  k :: UncurriedM t
  k args = do
    maybeR <- lookup args
    case maybeR of
      Just r  -> return r
      Nothing -> do
        r <- uncurryM f args
        insert args r
        return r

----------------------------------------------------------------
-- Unsafe versions.

-- Trace.

{-# NOINLINE levelRef #-}
levelRef :: IORef Int
levelRef = unsafePerformIO $ newIORef 0

-- We can give a short-ish signature using the 'UnsafePurifiable'
-- synonym.
unsafeTrace ::
  ( UnsafePurifiable n t
  , Show (Args n t)
  , Show (Ret n t) ) =>
  Proxy n -> String -> t -> t
unsafeTrace p name = unsafePurify p (return $ trace levelRef name)

-- Or GHC can just infer the type.
unsafeTrace' p name =
  unsafePurify p (return $ trace' get inc putStrLn name)
  where
    get = readIORef levelRef
    inc n = modifyIORef levelRef (+ n)

-- Memoize.

unsafeMemoize ::
  ( UnsafePurifiable n t
  , Ord (Args n t) )
  => Proxy n -> t -> t
unsafeMemoize p =
  unsafePurify p (memoize <$> newIORef Map.empty)

unsafeMemoize' p =
  unsafePurify p mkMemoize'
  where
    mkMemoize' = do
      cacheRef <- newIORef Map.empty
      let lookup args = Map.lookup args <$> readIORef cacheRef
      let insert args r = modifyIORef cacheRef (Map.insert args r)
      return $ memoize' lookup insert

-- Mutual recursion.
pow :: Int -> Int -> Int
pow = unsafeTrace $(proxyNat 2) "pow" pow' where
  pow' b p =
    if p <= 1
    then b * p
    else pow b (p `div` 2) * pow b (p - (p `div` 2))

-- Fixpoint.
openPow pow b p =
  if p <= 1
  then b * p
  else pow b (p `div` 2) * pow b (p - (p `div` 2))
pow'   = fix (unsafeTrace' $(proxyNat 2) "pow'" . openPow)
pow''  = fix (unsafeTrace  $(proxyNat 2) "pow"  . unsafeMemoize  $(proxyNat 2) . openPow)
pow''' = fix (unsafeTrace' $(proxyNat 2) "pow'" . unsafeMemoize' $(proxyNat 2) . openPow)

main :: IO ()
main = do
  putStrLn (pow    2 6 `seq` "")
  putStrLn (pow'   2 6 `seq` "")
  putStrLn (pow''  2 6 `seq` "")
  putStrLn (pow''' 2 6 `seq` "")
