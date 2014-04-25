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
-- Need 'PolyKinds' so that 'tag' in 'HetCall' can optionally have
-- non-'*' kind.
{-# LANGUAGE PolyKinds #-}

module Data.Function.Decorator.HetCall where

import GHC.Exts (Constraint)

import Data.Proxy

import Data.Function.Decorator
import Data.Function.Decorator.ConstraintLogic

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
  Proxy c -> Proxy tag -> Ex2T (LogTree (SigAll c)) -> Ex2T (LogTree c')
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

-- XXX: move this somewhere else.
--
-- Heterogeneous fold.
--
-- This is a step towards writing generic instances of post processors
-- which use a pretty printing function in the class 'c' (e.g. 'Show')
-- to format the '_arg's.

class HFold c t where
  hfoldl :: Proxy c -> (forall t'. c t' => a -> t' -> a) -> a -> t -> a
  hfoldr :: Proxy c -> (forall t'. c t' => t' -> a -> a) -> a -> t -> a

instance HFold c () where
  hfoldl _ _ x0 _ = x0
  hfoldr _ _ x0 _ = x0

instance (c t' , HFold c t) => HFold c (t',t) where
  hfoldl p f x0 (x , xs) = hfoldl p f (x0 `f` x) xs
  hfoldr p f x0 (x , xs) = x `f` hfoldr p f x0 xs

instance HFold c [H c] where
  hfoldl _ f = foldl f' where
    x0 `f'` H x = x0 `f` x
  hfoldr _ f = foldr f' where
    H x `f'` x0 = x `f` x0

hmap :: HFold c t => Proxy c -> (forall t'. c t' => t' -> a) -> t -> [a]
hmap p f = hfoldr p (\x as -> f x : as) []

formatCall :: HFold Show t => String -> t -> String
formatCall f xs =
  unwords $ f : hmap (Proxy::Proxy Show) (optionalParens . show) xs
  where
    -- Wrap in parens if the heuristic says we should.
    --
    -- The heuristic is "contains a space and doesn't look like a list
    -- (special case: String)".  I would just use
    --
    --   showsPrec 11 s ""
    --
    -- except that the default instance of 'showsPrec' ignores the
    -- precedence argument, so that wouldn't work for user defined
    -- (versus derived) 'Show' instances :P
    optionalParens s =
      if ' ' `elem` s &&
         (head s /= '[' || last s /= ']') &&
         (head s /= '"' || last s /= '"')
      then "(" ++ s ++ ")"
      else s
