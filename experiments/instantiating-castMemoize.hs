{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE ConstraintKinds #-}
{-# LANGUAGE UndecidableInstances #-}

module Main where

import Prelude hiding (lookup , curry)

import Control.Applicative
import Control.Monad.State
import qualified Data.Map.Strict as Map
import Data.IORef
import Data.Typeable

import Data.Function.Decorator.Memoizer
import Data.Function.Decorator.ConstraintLogic

-- For 'MonadIO'.
cacheRef :: IORef (Map.Map String (Maybe (H Typeable)))
cacheRef = undefined
lookup args = do
  cache <- liftIO $ readIORef cacheRef
  return $ Map.lookup args cache
insert args r =
  liftIO $ modifyIORef cacheRef (Map.insert args r)

-- For 'State'.  We also do 'State' in main 'Test.hs'.
lookup' args = do
  cache <- get
  return $ Map.lookup args cache
insert' args r =
  modify (Map.insert args r)