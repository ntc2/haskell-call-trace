{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE DataKinds #-}

module Debug.Trace.LogTree.Process.ProofTree where

import Data.List (intercalate)

import Debug.Trace.LogTree

class ProofTree call where
  callAndReturn :: LogTree ProofTree call "CallAndReturn" ->
    (String , String)
  callAndError  :: LogTree ProofTree call "CallAndError" ->
    (String , String)

proofTree :: Ex2T (LogTree ProofTree) -> String
proofTree (Ex2T t@(CallAndReturn {})) =
  "\\infer[ " ++ rule ++ " ]{ " ++ conclusion ++ " }{ " ++ intercalate " & " premises ++ " }"
  where
    (conclusion , rule) = callAndReturn t
    premises = map proofTree (_children t)
proofTree (Ex2T t@(CallAndError {})) =
  "\\infer[ " ++ rule ++ " ]{ " ++ conclusion ++ " }{ " ++ intercalate " & " premises ++ " }"
  where
    (conclusion , rule) = callAndError t
    premises = map proofTree (_children' t ++
                              maybe [] (:[]) (_how t))
