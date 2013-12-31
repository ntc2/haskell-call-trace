{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE DataKinds #-}

module RollMyOwnLiterals where

import Language.Haskell.TH

data Nat = Z | S Nat

nat :: Integer -> Q Type
nat 0 = [t|Z|]
nat n = [t|S $(nat (n-1))|]
