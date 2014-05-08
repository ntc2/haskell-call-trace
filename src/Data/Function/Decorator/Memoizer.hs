{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE ConstraintKinds #-}
{-# LANGUAGE UndecidableInstances #-}

module Data.Function.Decorator.Memoizer where

import Prelude hiding (lookup , curry)

import Control.Applicative
import Data.Typeable
-- XXX: Do I need to specify the strictness here? Probably I do, since
-- I'm going to insert empty dicts as needed, but would I still need
-- to specify strictness if the empty dicts were provided by the user
-- provided lookup function?
import qualified Data.Map.Strict as Map
import Text.Printf

import Data.Function.Decorator.ConstraintLogic
import Data.Function.Decorator.Curry

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
--     lookup :: Proxy call -> Args call -> m (Maybe (Ret call))
--     insert :: Proxy call -> Args call -> Ret call -> m ()
--
-- and then e.g.
--
--   data S = S { _fDict :: Data.Map (ArgsM FTy) (RetM FTy) , ... }
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
--   simpleMemoizer :: (Args a -> m (Maybe (Ret a))) ->    -- Insert
--                     (Args a -> Ret a -> m ()) ->        -- Lookup
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

-- Memoize a function using the given 'lookup' and 'insert' to cache
-- arguments and return values. In practice the 'lookup' and 'insert'
-- functions are specific to the memoized function.
simpleMemoize :: forall t. CurryUncurryM t
              => (ArgsM t -> MonadM t (Maybe (RetM t)))
              -> (ArgsM t -> RetM t -> MonadM t ())
              -> t
              -> t
simpleMemoize lookup insert f = curry k where
  k :: UncurriedM t
  k args = do
    maybeCached <- lookup args
    case maybeCached of
      Just ret -> return ret
      Nothing -> do
        ret <- uncurryM f args
        insert args ret
        return ret

-- Memoize a function using the given 'lookup' and 'insert' functions
-- to maintain a cache for this function.  In practice the 'lookup'
-- and 'insert' functions are generic, i.e. reused for many memoized
-- functions.  The 'tag' should be a globally unique (across all uses
-- of 'castMemoizer') identifier for the function 'f'.
--
-- The cache is maintained as 'Data.Map.Map' inside an 'H'
-- constructor.  We need to hide the map in a heterogeneous type
-- because we expect to store multiple caches via the same 'lookup'
-- and 'insert', and the different caches have different types.  The
-- 'Typeable' constraints allow us to 'cast' the maps after removing
-- them from the 'H' constructors.
castMemoize :: forall t.
             ( CurryUncurryM t
             , Ord (ArgsM t)
             , Typeable (ArgsM t)
             , Typeable (RetM t)
             , Functor (MonadM t) )
            => (String -> MonadM t (Maybe (H Typeable)))
            -> (String -> H Typeable -> MonadM t ())
            -> String
            -> t
            -> t
castMemoize lookup insert tag f = curry k where
  k :: UncurriedM t
  k args = do
    cache <- getCache
    case Map.lookup args cache of
      Just ret -> return ret
      Nothing -> do
        ret <- uncurryM f args
        -- Careful: we need to look up the cache again since the call
        -- may have mutated it.
        insert tag . H . Map.insert args ret =<< getCache
        return ret

  getCache :: MonadM t (Map.Map (ArgsM t) (RetM t))
  getCache =
    maybe Map.empty (unH castCache) <$> lookup tag

  castCache :: Typeable a => a -> Map.Map (ArgsM t) (RetM t)
  castCache d = case cast d of
    Just cache -> cache
    Nothing -> error msg where
      msg = printf ("Unable to cast cache in 'castMemoizer' with tag %s\n" ++
                    "This may indicate a tag collision!") tag
