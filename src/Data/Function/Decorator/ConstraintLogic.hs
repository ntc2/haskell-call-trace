{-# OPTIONS_GHC -fno-warn-unused-binds #-}
{-# OPTIONS_GHC -fno-warn-name-shadowing #-}

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
{-# LANGUAGE RankNTypes #-}

module Data.Function.Decorator.ConstraintLogic
  ((:&&:) , Implies' , coerceLogTree' , H(..) , unH)
where

import GHC.Prim (Constraint)

import Data.Proxy

import Data.Function.Decorator.Logger.LogTree

----------------------------------------------------------------
-- Conjunction of constraints.

infixr :&&:
class    (c1 t , c2 t) => (c1 :&&: c2) t
instance (c1 t , c2 t) => (c1 :&&: c2) t

class    Trivial t where
instance Trivial t where

----------------------------------------------------------------
-- Constraint implication.

-- We can't directly have "c1 a => c2 a" since constraints are not
-- first class.  However, we can essentially make them first class by
-- storing evidence for them in a data type. Data types are first
-- class, so this allows us to *return* the evidence, which lets us
-- use the function space arrow '->' for implication.
data Reify c a where
  Reify :: c a => Reify c a

-- Implication then becomes the ability to translate evidence.
--
-- In practice, all concrete instances of 'Implies c1 c2' are
--
--   \case Reify -> Reify
--
-- !
type Implies c1 c2 = forall a. Reify c1 a -> Reify c2 a

-- The reason we want constraint implication!
coerceLogTree :: forall c1 c2.
  Implies c1 c2 -> Ex2T (LogTree c1) -> Ex2T (LogTree c2)
-- There are a few subtleties here:
--
-- - The actual constraint needed for constructing 'LogTree c' is
--   'SigWith c call', not 'c call', but
--
--     SigWith c call ~ (Signature call , c call)
--
--   and so we can reuse the 'Signature call' constraint which comes
--   into scope when we unpack the 'LogTree c' object, and we then we
--   only have to cast 'c1 call' to 'c2 call'.
--
-- - To do the cast we need to mention the type of the call (also
--   called 'call', hope that's not too confusing) in the type of
--   'Reify' object we pass to 'impl'.  I don't know any way to scope
--   this in the type signature itself, but using a local signature
--   '(call::call)' on the call argument we can scope. I didn't know
--   GHC supported such local signatures on arguments until I tried it
--   ... not sure what I would have done if it hadn't worked ...
coerceLogTree impl (Ex2T (CallAndReturn (call::call) before args children ret after)) =
  case impl (Reify::Reify c1 call) of
    Reify -> Ex2T $ CallAndReturn call before args children' ret after
  where
    children' = map (coerceLogTree impl) children
coerceLogTree impl (Ex2T (CallAndError (call::call) before args children who)) =
  case impl (Reify::Reify c1 call) of
    Reify -> Ex2T $ CallAndError call before args children' who'
  where
    children' = map (coerceLogTree impl) children
    who' = fmap (coerceLogTree impl) who

-- And using the other definition of implication.
coerceLogTree' :: forall c1 c2.
  Implies' c1 c2 -> Ex2T (LogTree c1) -> Ex2T (LogTree c2)
-- XXX: need to come back to this and understand how much of this
-- complexity is necessary.  Essentially, I ended up giving nearly
-- every computation and explicit type sig, and specialized the types
-- of 'impl' and the 'LogTree' constructors.  Note that, without all
-- this signature stuff, the definitions are dead simple:
--
--  coerch impl (Con a1 ... an) = impl Con a1 .. an !!!
--
-- The need to
--
-- I found the error messages I got without the signatures
-- confusing. They complain about failing to unify "rigid" and
-- "untouchable" variables. E.g., when I had no sig on 'children'' I
-- got:
--
--     Couldn't match type `c20' with `c2'
--       `c20' is untouchable
--             inside the constraints (c1 a)
--             bound at a type expected by the context:
--                        (c1 a) => ((c20 a) => b) -> b
--       `c2' is a rigid type variable bound by
--            the type signature for
--              coerceLogTree' :: Implies' c1 c2 -> LogTree c1 -> LogTree c2
--            at Debug/Trace/LogTree/ConstraintLogic.hs:84:29
--     Expected type: ((c20 a) => b) -> b
--       Actual type: ((c2 a) => b) -> b
--     In the first argument of coerceLogTree', namely `impl'
--     In the first argument of `map', namely `(coerceLogTree' impl)'
--     In the expression: map (coerceLogTree' impl) children
--
-- What is an "untouchable" variable?
coerceLogTree' impl (Ex2T (CallAndReturn (call::call) before args children ret after)) =
  impl' callAndReturn call before args children' ret after
  where
    callAndReturn :: c2 call =>
      call -> Before call -> Args call -> LogForest c2 -> Ret call -> After call
           -> Ex2T (LogTree c2)
    callAndReturn = ((.).(.).(.).(.).(.).(.)) Ex2T CallAndReturn

    impl' :: forall b. (c2 call => b) -> (c1 call => b)
    impl' = impl

    children' :: LogForest c2
    children' = map (coerceLogTree' impl) children
coerceLogTree' impl (Ex2T (CallAndError (call::call) before args children who)) =
  impl' callAndError call before args children' who'
  where
    callAndError :: c2 call =>
      call -> Before call -> Args call -> LogForest c2 -> Maybe (Ex2T (LogTree c2))
           -> Ex2T (LogTree c2)
    callAndError = ((.).(.).(.).(.).(.)) Ex2T CallAndError

    impl' :: forall b. (c2 call => b) -> (c1 call => b)
    impl' = impl

    children' :: LogForest c2
    children' = map (coerceLogTree' impl) children

    who' :: Maybe (Ex2T (LogTree c2))
    who' = fmap (coerceLogTree' impl) who

----------------------------------------------------------------

-- Also, it's easy to create specify and abstract implications, given
-- the constraint structure:
implicationTest1 :: Implies (Show :&&: Eq) Show
implicationTest1 Reify = Reify
implicationTest2 :: forall c1 c2 c3. Implies (c1 :&&: c2 :&&: c3) c2
implicationTest2 Reify = Reify

-- XXX: could make an implies class (e.g. 'c1 :=>: c2'), something
-- like
--
--   class c1 :=>: c2 where
--     impl :: Implies c1 c2
--
--   instance c :=>: c where
--     impl = id
--   instance (c1 :&&: c2) :=>: c1 where
--     impl Reify = Reify
--   instance (c1 :&&: c2) :=>: c2 where
--     impl Reify = Reify
--   instance (c1 :&&: c2 :&&: c3) :=>: c1 where
--     impl Reify = Reify
--
-- etc.  It would be very easy to generate these instances
-- programmatically, although I doubt there'd be much use for greater
-- than three conjuncts, at least for logging.
--
-- That would make the interface to the log processors a little more
-- friendly, although passing the 'Implies' argument is pretty easy:
-- for any concrete pair of constraints the argument is '\case Reify
-- -> Reify' :P And it's even easier for 'Implies'', where the
-- argument is always '\x -> x'.  That's an argument in favor of using
-- 'Implies'', since then no one ever has to know about 'Reify'.
--
-- On the other hand, the 'coerceLogTree' examples show that, at least
-- there, 'Implies' and 'Reify' are much easier to use than
-- 'Implies'', since the former requires only minimal type sigs
-- (specify which 'Reify' we want).  It may be true in general that
-- 'Reify' and 'Implies' are more friendly: they deal directly with
-- manipulating constraints in the context, whereas 'Implies'' is
-- about manipulating constraints in the types of functions.

----------------------------------------------------------------
-- Alternate definition of constraint implication.

-- Now we are using 'c1' and 'c2' in a contravariant position, so the
-- order gets reversed.
--
-- In practice, all concrete instances of 'Implies' c1 c2' are '\x ->
-- x'!
type Implies' c1 c2 = forall a b. (c2 a => b) -> (c1 a => b)

-- Proofs of equivalence.
implies1 :: forall c1 c2. Implies c1 c2 -> Implies' c1 c2
implies2 :: forall c1 c2. Implies' c1 c2 -> Implies c1 c2
implies1 = f where
  -- I can't figure out how to make this type check without manually
  -- unfolding the signature and moving the constraint 'c1 a'.  In
  -- particular, I need to scope the type variable 'a', which I don't
  -- get with the alias.  This seems to be a short coming in the
  -- interaction between type functions and scoped type variables.
  -- f :: forall c1 c2 a b. c1 a => Implies c1 c2 -> (c2 a => b) -> b
  f :: forall c1 c2 a b. Implies c1 c2 -> (c2 a => b) -> (c1 a => b)
  f impl x = case impl (Reify::Reify c1 a) of Reify -> x
implies2 impl = f impl where
  -- I also have to to expand here, and moreover I manually
  -- instantiate type params ('b' to 'Reify c2 a') and make the arg
  -- 'impl' explicit to implicit instantiate it's type params.
  f :: forall c1 c2 a
     . ((c2 a => Reify c2 a) -> (c1 a => Reify c2 a))
    -> Reify c1 a -> Reify c2 a
  f impl x = case x of
    Reify -> impl (Reify::c2 a => Reify c2 a)

implicationTest1' :: Implies' (Show :&&: Eq) Show
implicationTest1' f = f
implicationTest2' :: forall c1 c2 c3. Implies' (c1 :&&: c2 :&&: c3) c2
implicationTest2' f = f

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
--
-- UPDATE: the 'Reify' version above is better, since we also track
-- the underlying type.
class C'' cs c where
  c'' :: cs a => a -> H c
instance C' cs c => C'' cs c where
  c'' x = c' (H x::H cs)

----------------------------------------------------------------
-- Coercion of constraints.

-- XXX: move this somewhere else, maybe with the 'HetCall' code.
data H (c :: * -> Constraint) where
  H :: c a => a -> H c

unH :: (forall a. c a => a -> b) -> H c -> b
unH f (H x) = f x

coerceH :: forall c1 c2. Implies c1 c2 -> H c1 -> H c2
coerceH impl (H (x :: a)) =
  case impl (Reify :: Reify c1 a) of
    Reify -> H x

coerceH' :: forall c1 c2. Implies' c1 c2 -> H c1 -> H c2
coerceH' impl (H (x :: a)) = impl' H x where
  impl' :: forall b. (c2 a => b) -> (c1 a => b)
  impl' = impl

{-
data Where = Here | LeftLater Where | RightLater Where

class CoerceH (w::Where) cs c where
  coerceH :: Proxy w -> H cs -> H c

instance CoerceH Here c c where
  coerceH _ h = h

instance CoerceH w cs c => CoerceH (LeftLater w) (cs :&&: c') c where
  coerceH _ h =
    coerceH (Proxy::Proxy w) (pi1 h)

instance CoerceH w cs c => CoerceH (RightLater w) (c' :&&: cs) c where
  coerceH _ h =
    coerceH (Proxy::Proxy w) (pi2 h)
-}