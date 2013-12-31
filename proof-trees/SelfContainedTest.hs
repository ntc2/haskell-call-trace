-- Type check and build natural deduction proof tree.
--
-- This is like 'Test', but does not use 'LogTree'.

{-# OPTIONS_GHC -fno-warn-name-shadowing #-}
{-# LANGUAGE NamedFieldPuns #-}

module SelfContainedTest where

import Control.Applicative ((<$>))
import Control.Monad.Reader
import Data.List (intercalate)

import Test hiding (main , pipeline , M , Assoc(..) , runM)

----------------------------------------------------------------
-- Type checker + proof builder.

-- The mode is 'True' if proof terms should be included.
data R = R { _ctx :: Ctx , _mode :: Bool }
type M a = Reader R a
runM :: R -> M a -> a
runM r = flip runReader r

extendCtx :: TmVar -> Ty -> M a -> M a
extendCtx x t = local extend where
  extend r = r { _ctx = _ctx r ++ [(x,t)] }

-- These take the place of the inferred type when there is a type
-- error.
here , there :: String
here = "\\,!"
there = "\\,\\uparrow"

-- Return the inferred type---or error string if type inference
-- fails---and the latex proof-tree presentation of the inference.
inferProof :: Tm -> M (Either String Ty , String)
inferProof tm@(Lam x t e) = do
  (et' , p) <- extendCtx x t . inferProof $ e
  let et'' = (t :->:) <$> et'
  addProof et'' [p] tm
inferProof tm@(TmVar x) = do
  mt <- lookup x <$> asks _ctx
  let et = maybe (Left here) Right mt
  addProof et [] tm
inferProof tm@(e :@: e1) = do
  (et , p) <- inferProof e
  (et1 , p1) <- inferProof e1
  case (et , et1) of
    (Right t , Right t1) ->
      case t of
        t1' :->: t2 | t1' == t1 -> addProof (Right t2)   [p , p1] tm
        _ ->                       addProof (Left here)  [p , p1] tm
    _ ->                           addProof (Left there) [p , p1] tm

-- Given the inferred type, the proof-trees for all premise inferences
-- (subcalls), and the input term, annotate the inferred type with a
-- result proof tree.
addProof :: Either String Ty -> [String] -> Tm -> M (Either String Ty , String)
addProof et premises tm = do
  R { _mode , _ctx } <- ask
  let (judgment , rule) = conclusion _mode _ctx tm et
  let tex = "\\infer[ " ++ rule ++ " ]{ " ++
            judgment ++ " }{ " ++
            intercalate " & " premises ++ " }"
  return (et , tex)

----------------------------------------------------------------
-- Bring it all together.

pipeline :: Mode -> Ctx -> String -> String
pipeline mode ctx =
    snd
  . runM (R { _ctx = ctx , _mode = mode })
  . inferProof
  . callParser tm

main :: IO ()
main = mainWith pipeline
