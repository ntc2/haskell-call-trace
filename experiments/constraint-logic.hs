{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE KindSignatures #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE ConstraintKinds #-}
{-# LANGUAGE UndecidableInstances #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE OverlappingInstances #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeFamilies #-}

module Debug.Trace.LogTree.ConstraintLogic where

import GHC.Prim (Constraint)

----------------------------------------------------------------
-- Unused constraint logic stuff.

-- XXX: I can't partially apply a synonym:
{-
type ShowWith c t = (Show t , c t)
-}
-- Did I work around this in a similar example when I was doing the
-- het lists for Max? The point is that I'd like a 'Show' instance
-- that works for 'LogTree c' with any constraint 'c' that implies
-- 'Show'.
--
-- Making a some special classes kind of works, e.g.:
class    (c1 t , c2 t) => (c1 :&&: c2) t
instance (c1 t , c2 t) => (c1 :&&: c2) t
-- but this doesn't really give implication. We can almost get
-- implication for constraints built from the combinators with

-- ??? Is there some way to existentially quantifier a class parameter
-- ??? This does not work: 'Not in scope: type variable `p''
{-
class c p => Exists c
instance c p => Exists c
-}

data Where = Here | L Where | R Where
class ((c :: * -> Constraint) `Elem` (cs :: * -> Constraint)) (evidence::Where)
instance (c `Elem` c) Here
instance (c `Elem` cs) evidence => (c `Elem` (c' :&&: cs)) (R evidence)
instance (c `Elem` cs) evidence => (c `Elem` (cs :&&: c')) (L evidence)

{-
type family   Lookup c cs (evidence::Where) :: *
type instance Lookup c c  Here = c
type instance (c `Elem` cs) evidence => 
-}

-- class Exists (c2 `Elem` c1) => (c1 :: * -> Constraint) :=>: (c2 :: * -> Constraint)
-- instance c :=>: c
-- instance (c1 :=>: c) => (c1 :&&: c2) :=>: c
{-
instance (c2 :=>: c) => (c1 :&&: c2) :=>: c
-}
{-
data Side = L | R
class (c1 :||: c2) (which::Side) t
instance c1 t => (c1 :||: c2) L t
instance c2 t => (c1 :||: c2) R t
-}
-- except the last instance overlaps with the previous one, so we
-- might need something like instance chains here.
