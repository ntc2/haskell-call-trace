{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE MultiParamTypeClasses #-}

module Data.Function.Decorator.Logger.Processor.ProofTree where

import Data.List (intercalate)

import Data.Function.Decorator.Logger.LogTree

class ProofTree mode call where
  callAndReturn :: mode -> LogTree (ProofTree mode) call "CallAndReturn" ->
    (String , String)
  callAndError  :: mode -> LogTree (ProofTree mode) call "CallAndError" ->
    (String , String)

proofTree :: mode -> Ex2T (LogTree (ProofTree mode)) -> String
proofTree mode (Ex2T t@(CallAndReturn {})) =
  "\\infer[ " ++ rule ++ " ]{ " ++ conclusion ++ " }{ " ++ intercalate " & " premises ++ " }"
  where
    (conclusion , rule) = callAndReturn mode t
    premises = map (proofTree mode) (_children t)
proofTree mode (Ex2T t@(CallAndError {})) =
  "\\infer[ " ++ rule ++ " ]{ " ++ conclusion ++ " }{ " ++ intercalate " & " premises ++ " }"
  where
    (conclusion , rule) = callAndError mode t
    premises = map (proofTree mode)
                   (_children' t ++ maybe [] (:[]) (_how t))
