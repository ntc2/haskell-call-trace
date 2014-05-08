{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE ConstraintKinds #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE OverlappingInstances #-}

{-

An experiment to see if closed type families in GHC 7.8 could be used
to (conceptually) simplify the definition of 'UncurryM'. Conclusion:
sort of.

The closed type families mean we can define the 'UncurryM' type
functions 'ArgsM' and friends in a more straightforward way, but the
*term* function 'uncurryM' still needs to be defined in the type class
'UncurryM', and so we end up with overlapping instances anyway :P
Seems we'd also need "closed type classes" to avoid all overlap ...

However, using '-XOverlappingInstances' the code runs and doe the
right thing.  I recall that in 7.6 this did not work, but I don't
remember why. It would be interesting to see if the naive overlapping
7.6 code "just works" in 7.8 with overlapping instances ...

I don't have GHC 7.8 installed yet, but I was able to run the example
in the FP Complete Haskell Center online Haskell IDE: cool!

https://www.fpcomplete.com/business/haskell-industry/

One strange thing about 7.8: the overlapping instances are not caught
at the definition sites, but rather at the use site in
'fancyReturn'. In 7.6 I believe they are caught at the definition
sites. So, it seems 7.8 may treat overlapping instances differently.

-}

module Main where

import Prelude hiding (curry)

{-
class UncurryM (t :: *) where
    type ArgsM t :: *
    type RetM t :: *
    type MonM t :: *
-}
type family ArgsM (t :: *) :: * where
  ArgsM (a -> b) = (a , ArgsM b)
  ArgsM (m b) = ()

type family RetM (t :: *) :: * where
  RetM (a -> b) = RetM b
  RetM (m b) = b

type family MonM (t :: *) :: * -> * where
  MonM (a -> b) = MonM b
  MonM (m b) = m

type UncurriedM (t :: *) = ArgsM t -> MonM t (RetM t)

class Monad (MonM t) => UncurryM (t :: *) where
  uncurryM :: t -> UncurriedM t

instance UncurryM b =>
  UncurryM (a -> b) where
  uncurryM f (x , xs) = uncurryM (f x) xs

instance (Monad m , MonM (m b) ~ m , ArgsM (m b) ~ () , RetM (m b) ~ b) =>
  UncurryM (m b) where
  uncurryM f () = f

type family (as :: *) ->* (b :: *) where
  () ->* b = b
  (a , as) ->* b = a -> as ->* b

class Curry (as :: *) (b :: *) where
  curry :: (as -> b) -> (as ->* b)

instance Curry () b where
  curry f = f ()
instance Curry as b => Curry (a , as) b where
  curry f x = curry (\xs -> f (x , xs))

-- An identity decorator.
d :: forall t.
  ( UncurryM t
  , Curry (ArgsM t) (MonM t (RetM t))
  , (ArgsM t ->* MonM t (RetM t)) ~ t ) =>
  t -> t
d f = curry k where
  k :: UncurriedM t
  k args = do
    r <- uncurryM f args
    return r

fancyReturn :: a -> IO a
fancyReturn = d return

main = do
  x <- fancyReturn 42
  print x

-- Without '-XOverlappingInstances':
{-
src/Main.hs@69:15-69:16
Overlapping instances for UncurryM (a -> IO a) arising from a use of ‛d’
Matching instances: instance UncurryM b => UncurryM (a -> b)
-- Defined at /home/app/isolation-runner-work/projects/38371/src.207/Main.hs:36:10
instance (Monad m, MonM (m b) ~ m, ArgsM (m b) ~ (), RetM (m b) ~ b) => UncurryM (m b)
-- Defined at /home/app/isolation-runner-work/projects/38371/src.207/Main.hs:40:10
In the expression: d return
In an equation for ‛fancyReturn’: fancyReturn = d return
-}
