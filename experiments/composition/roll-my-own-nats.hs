{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE KindSignatures #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE ScopedTypeVariables #-}

module RollMyOwnNats where

import Data.List (genericIndex)

-- import Data.Proxy
data Proxy (n::Nat) = Proxy

----------------------------------------------------------------
-- Stuff that works.

data Nat = Z | S Nat

class Compose (n::Nat) b b' t t' where
  compose :: Proxy n -> (b -> b') -> t -> t'

instance Compose Z b b' b b' where
  compose _ f x = f x

instance Compose n b b' t t' => Compose (S n) b b' (a -> t) (a -> t') where
  compose _ g f x = compose (Proxy::Proxy n) g (f x)

-- Complement a binary relation.
compBinRel :: (a -> a -> Bool) -> (a -> a -> Bool)
compBinRel = compose (Proxy::Proxy (S (S Z))) not

----------------------------------------------------------------
-- Stuff that does not work.

instance Num Nat where
  fromInteger n = iterate S Z `genericIndex` n
-- I now have 'Nat' literals:
myTwo :: Nat
myTwo = 2
-- But GHC thinks my type-level nat literal is a 'GHC.TypeLits.Nat',
-- even when I say otherwise:
compBinRel' :: (a -> a -> Bool) -> (a -> a -> Bool)
compBinRel' = compose (Proxy::Proxy (2::Nat)) not
{-
    Kind mis-match
    An enclosing kind signature specified kind `Nat',
    but `2' has kind `GHC.TypeLits.Nat'
    In an expression type signature: Proxy (2 :: Nat)
    In the first argument of `compose', namely
      `(Proxy :: Proxy (2 :: Nat))'
    In the expression: compose (Proxy :: Proxy (2 :: Nat)) not
-}
