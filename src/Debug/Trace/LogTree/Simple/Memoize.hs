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
-- The pattern for lightweight memoization is that
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
-- points here is that we need to memoize away from the definition
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
-- a default instance for the state monad that stores a single dictionary,
-- reducing the boilerplate to one line per function.


-- Note: the 'GetArg t `Curried` GetMonad t (GetRet t)' is just a
-- fancy way to write 't' (that GHC prefers).
simpleMemoizer :: forall t. CollectAndCallCont t
               => (GetArg t -> GetMonad t (Maybe (GetRet t)))
               -> (GetArg t -> GetRet t -> GetMonad t ())
               -> t
               -> GetArg t `Curried` GetMonad t (GetRet t)
simpleMemoizer lookup insert f = collectAndCallCont k f where
  k :: (GetArg t , GetMonad t (GetRet t)) -> GetMonad t (GetRet t)
  k (arg , mret) = do
    maybeCached <- lookup arg
    case maybeCached of
      Just ret -> return ret
      Nothing -> do
        ret <- mret
        insert arg ret
        return ret
