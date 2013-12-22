{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE FlexibleContexts #-}

module Debug.Trace.LogTree.Simple.Curry where

import Debug.Trace.LogTree.SimpleCall

import Control.Monad.Identity

----------------------------------------------------------------

{-
class Curry a b where
  type Curried a b
  collectCall :: (t ~ Curried a b , UncurryM t)
              => t -> Curried (GetArg t) (GetMonad t (GetRet t) , GetArg t)

instance (Monad m , Monad (t m)) => Curry () (t m r) where
  type Curried () (t m r) = t m r
  collectCall tmr = (tmr , ())
-}

class Curry a b where
  type Curried a b

instance Curry () b where
  type Curried () b = b

instance Curry as b => Curry (a , as) b where
  type Curried (a , as) b = a -> Curried as b

class UncurryM t => Collect t where
  collectCall :: t -> Curried (GetArg t) (GetMonad t (GetRet t) , GetArg t)

instance (Monad m , Monad (t m)) => Collect (t m r) where
  collectCall tmr = (tmr , ())

instance Collect (Identity r) where
  collectCall i = (i , ())

{-
instance Collect b => Collect (a -> b) where
  collectCall f a = <need an accumulator!>
-}

----------------------------------------------------------------
