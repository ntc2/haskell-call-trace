{-# LANGUAGE DataKinds #-}
{-# LANGUAGE KindSignatures #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE GADTs #-}

import Data.Proxy

import GHC.TypeLits

import Data.Typeable (Typeable)

data A = A
  deriving Typeable

data B a = B a
  deriving Typeable

{-
data C (a::Symbol) = C
  deriving Typeable

    Can't make a derived instance of `Typeable (C a)':
      `C' must only have arguments of kind `*'
    In the data declaration for `C'
-}

data D a where
  DZero :: D ()
  DSucc :: D a -> D ((),a) 
  deriving Typeable

data H = H0 | H1 | H2
  deriving Typeable

{-
data W a where
  W0 :: W H0
  deriving Typeable

    Can't make a derived instance of `Typeable (W a)':
      `W' must only have arguments of kind `*'
    In the data declaration for `W'
-}

data H0
data H1
data H2
data H3
data H4
data H5
data H6
data H7
data H8
data H9
data Ha
data Hb
data Hc
data Hd
data He
data Hf

data HByte h l

data B0 deriving Typeable
data B1 deriving Typeable

{-
data S a where
  Sdeadbeef :: S (Byte Hd He , (Byte Ha Hd , (Byte Hb He , (Byte He Hf , ()))))
  deriving Typeable
-}

data S a where
  SNil :: S ()
  SCons :: Proxy b -> S a -> S (b , a)
  deriving Typeable

-- Goal: compute a (necessarily existentially quantfied) unique type
-- from a string.  I'm using hex, but binary is probably simpler; not
-- like I'm going to read these types anyway ...

data Ex p where
  Ex :: (Typeable a , Typeable (p a)) => p a -> Ex p
  -- deriving Typeable

{-
    Can't make a derived instance of `Typeable (Ex p)':
      `Ex' must only have arguments of kind `*'
    In the data declaration for `Ex'
-}

string2Type :: String -> Ex S
string2Type "" = Ex SNil
string2Type (c:cs) = case string2Type cs of
  Ex e -> Ex $ SCons (Proxy::Proxy B0) e


-- -- deadbeef :: Proxy (S 