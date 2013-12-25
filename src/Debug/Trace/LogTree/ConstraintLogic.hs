{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE ConstraintKinds #-}
{-# LANGUAGE UndecidableInstances #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE KindSignatures #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE FlexibleContexts #-}

module Debug.Trace.LogTree.ConstraintLogic where

import GHC.Prim (Constraint)

import Data.Proxy

----------------------------------------------------------------
-- Conjunction of constraints.

infixr :&&:
class    (c1 t , c2 t) => (c1 :&&: c2) t
instance (c1 t , c2 t) => (c1 :&&: c2) t

class    Trivial t where
instance Trivial t where

----------------------------------------------------------------
-- Projection of conjunctions.

-- XXX: use TypeLits nats instead.

data Nat = Z | S Nat
type family   Project (n :: Nat) (c :: * -> Constraint) :: * -> Constraint
type instance Project Z          (c :&&: cs) = c
type instance Project (S n)      (c :&&: cs) = Project n cs

-- Trick for testing type computations: add the computation as a
-- constraint and then try to run the function in GHCi.  If GHC can
-- compute the function at compile time it will, but if it can't,
-- running it will force GHC to tell you. See commented 'test5' below.

test1 :: Project Z (Show :&&: Eq) ~ Show => Int
test1 = 3

-- Fails as expected.
-- test2 :: Project Z (Eq :&&: Show) ~ Show => Int
-- test2 = 3

test3 :: Project (S Z) (Show :&&: Eq) ~ Eq => Int
test3 = 3

test4 :: Project (S (S Z)) (Show :&&: Eq :&&: Ord :&&: Trivial) ~ Ord => Int
test4 = 3

{-
    Couldn't match type `Ord' with `Eq'
    Inaccessible code in
      the type signature for
        test5 :: Project
                   ('S ('S 'Z)) (Show :&&: (Eq :&&: (Ord :&&: Trivial)))
                 ~ Eq =>
                 Int

test5 :: Project (S (S Z)) (Show :&&: Eq :&&: Ord :&&: Trivial) ~ Eq => Int
test5 = 3
-}

pi1 :: H (c1 :&&: c2) -> H c1
pi1 (H x) = H x

pi2 :: H (c1 :&&: c2) -> H c2
pi2 (H x) = H x

class Project n cs ~ c => C n cs c where
  cH :: Proxy n -> H cs -> H c
  -- cH' :: H cs -> H c

instance C Z (c :&&: cs) c where
  cH _ (H x) = H x
  -- cH' h = h

instance C n cs c => C (S n) (c' :&&: cs) c where
  cH _ = cH (Proxy::Proxy n) . pi2
  -- cH' :: H (c' :&&: cs) -> H c
  -- cH' = (cH' :: C n cs c => H cs -> H c). pi2

h :: H (Show :&&: Ord :&&: Trivial)
h = H (3::Int)

h' :: H Ord
h' = cH (Proxy::Proxy (S Z)) h

----------------------------------------------------------------
-- Idea: coerce before use? The point is to get a general coerce
-- function that's not specialized to a specific container.

class C' cs c where
  c' :: H cs -> H c
instance C n cs c => C' cs c where
  c' h = cH (Proxy::Proxy n) h

-- Class 'c1' implies class 'c2' if there is a function
--
--   :: forall a. c1 a => a -> H c2
--
-- ??? Not quite: we'd also like to know that the underlying type did
-- not change, although for our purposes it may not matter, since the
-- whole point is that we work in a context where the type is
-- existentially quantified and the class is all we have to work with.
-- Note also the 'H' is 'Ex' for classes.
class C'' cs c where
  c'' :: cs a => a -> H c
instance C' cs c => C'' cs c where
  c'' x = c' (H x::H cs)

----------------------------------------------------------------
-- Coercion of constraints.

data H c where
  H :: c a => a -> H c

data Where = Here | LeftNow | LeftLater Where | RightNow | RightLater Where

class CoerceH (w::Where) cs c where
  coerceH :: Proxy w -> H cs -> H c

instance CoerceH Here c c where
  coerceH _ h = h

instance CoerceH LeftNow (c :&&: c') c where
  coerceH _ (H x) = H x

instance CoerceH RightNow (c' :&&: c) c where
  coerceH _ (H x) = H x

instance CoerceH w cs c => CoerceH (LeftLater w) (cs :&&: c') c where
  coerceH _ h =
    coerceH (Proxy::Proxy w)
      (coerceH (Proxy::Proxy LeftNow) h :: H cs)

instance CoerceH w cs c => CoerceH (RightLater w) (c' :&&: cs) c where
  coerceH _ h =
    coerceH (Proxy::Proxy w)
      (coerceH (Proxy::Proxy RightNow) h :: H cs)



{-
f :: CoerceH w (c1 :&&: c2 :&&: c3 :&&: c4) c1 => H (c1 :&&: c2) -> H c1
f = coerceH Proxy
-}

{-
instance CoerceH w cs c => CoerceH (LeftLater w) (cs :&&: c') c where
  coerceH (H x) =
    case (coerceH (H x) :: H cs) of
      H y -> coerceH (H y)
-}

{-
instance CoerceH Here (c :&&: cs) c where
  coerceH (H x) = H x

instance CoerceH w cs c => CoerceH (There w) (c' :&&: cs) c where
  coerceH (H x) = (coerceH :: ) (H x)
-}