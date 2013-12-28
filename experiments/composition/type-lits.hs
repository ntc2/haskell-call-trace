{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE KindSignatures #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE UndecidableInstances #-}

module UseGHCTypeLitsNats where

import GHC.TypeLits

-- import Data.Proxy
data Proxy (t::Nat) = Proxy

----------------------------------------------------------------
-- Stuff that works.

class Compose (n::Nat) b b' t t' where
  compose :: Proxy n -> (b -> b') -> t -> t'

instance Compose 0 b b' b b' where
  compose _ f x = f x

instance (Compose n b b' t t' , sn ~ (1 + n)) => Compose sn b b' (a -> t) (a -> t') where
  compose _ g f x = compose (Proxy::Proxy n) g (f x)

----------------------------------------------------------------
-- Stuff that does not work.

-- Complement a binary relation.
compBinRel , compBinRel' :: (a -> a -> Bool) -> (a -> a -> Bool)
compBinRel = compose (Proxy::Proxy 2) not
{-
    Couldn't match type `1 + (1 + n)' with `2'
    The type variable `n' is ambiguous
    Possible fix: add a type signature that fixes these type variable(s)
    In the expression: compose (Proxy :: Proxy 2) not
    In an equation for `compBinRel':
        compBinRel = compose (Proxy :: Proxy 2) not
-}
{-
    No instance for (Compose n Bool Bool Bool Bool)
      arising from a use of `compose'
    The type variable `n' is ambiguous
    Possible fix: add a type signature that fixes these type variable(s)
    Note: there is a potential instance available:
      instance Compose 0 b b' b b'
-}
compBinRel' = compose (Proxy::Proxy (1+(1+0))) not
{-
    Couldn't match type `1 + (1 + 0)' with `1 + (1 + n)'
    NB: `+' is a type function, and may not be injective
    The type variable `n' is ambiguous
    Possible fix: add a type signature that fixes these type variable(s)
    Expected type: Proxy (1 + (1 + 0))
      Actual type: Proxy (1 + (1 + n))
    In the first argument of `compose', namely
      `(Proxy :: Proxy (1 + (1 + 0)))'
-}
