{-# LANGUAGE DataKinds #-}
{-# LANGUAGE KindSignatures #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE ConstraintKinds #-}
{-# LANGUAGE UndecidableInstances #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE GADTs #-}

module Debug.Trace.LogTree.HetCall where

import GHC.Exts (Constraint)

import Data.Proxy

import Debug.Trace.LogTree
import Debug.Trace.LogTree.ConstraintLogic

----------------------------------------------------------------
-- A call-signature type for signatures with all parts in the same
-- class 'c'.

-- Here the 'tag' is used to distinguish different 'HetCall c' for the
-- same 'c'.  We want to write uniform processor instances that work
-- for a family of functions, so the 'tag' here specifies the family,
-- not an individual function in the family, as in 'SimplCall's 'tag'.
data HetCall (c:: * -> Constraint) tag = HetCall String

instance Signature (HetCall c tag) where
  name (HetCall s) = s
  type Before (HetCall c tag) = H c
  -- The argument tuple is unfolded into a list of hets.
  type Arg    (HetCall c tag) = [H c]
  type Ret    (HetCall c tag) = H c
  type After  (HetCall c tag) = H c

----------------------------------------------------------------

-- A signature with all parts satisfying the same constraint.  This
-- would just be a constraint synonym, except synonyms can't be
-- partially applied :P
class    (Signature call , c (Before call) , HFold c (Arg call) , c (Ret call) , c (After call))
         => SigAll c call
instance (Signature call , c (Before call) , HFold c (Arg call) , c (Ret call) , c (After call))
         => SigAll c call

-- Make all calls into 'HetCall c tag' calls, while changing processor
-- type to 'c''.
--
-- Can't use 'Proxy (c' , tag)' here in the type, I guess because
-- lifted tuples require both args to be of the same kind?
heterogenize :: forall c' c tag. c' (HetCall c tag) =>
  Proxy c' -> Proxy tag -> Ex2T (LogTree (SigAll c)) -> Ex2T (LogTree c')
heterogenize p1 p2 (Ex2T (CallAndReturn {..})) =
  Ex2T (CallAndReturn (HetCall (name _call)::HetCall c tag)
                      (H _before)
                      (hmap (Proxy::Proxy c) H _arg)
                      (map (heterogenize p1 p2) _children)
                      (H _ret)
                      (H _after))
heterogenize p1 p2 (Ex2T (CallAndError {..})) =
  Ex2T (CallAndError (HetCall (name _call')::HetCall c tag)
                     (H _before')
                     (hmap (Proxy::Proxy c) H _arg')
                     (map (heterogenize p1 p2) _children')
                     (fmap (heterogenize p1 p2) _how))

----------------------------------------------------------------
