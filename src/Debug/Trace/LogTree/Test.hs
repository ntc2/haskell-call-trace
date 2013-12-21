{-# LANGUAGE FlexibleInstances
           , MultiParamTypeClasses
           , ConstraintKinds
           , UndecidableInstances
           , TypeOperators
           -- , KindSignatures
           , TypeFamilies
           , StandaloneDeriving
           -- , ExistentialQuantification
           #-}


module Debug.Trace.LogTree.Test where

import GHC.Prim (Constraint)

import Control.Monad.Trans.Maybe
import Control.Monad.Writer

import Text.Parsec

import Debug.Trace.LogTree

----------------------------------------------------------------

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
class (c1 t , c2 t) => (c1 :&&: c2) t
instance (c1 t , c2 t) => (c1 :&&: c2) t

class Unit t
instance Unit t

instance Show (LogTree (Show :&&: c)) where
  show (CallAndReturn call _ ls _) =
    "<CallAndReturn (" ++ show call ++ ") _ " ++ show ls ++ " _>"
  show (CallAndError call _ ls e) =
    "<CallAndError (" ++ show call ++ ") _ " ++ show ls ++ " (" ++ show e ++ ")>"
{-
f , f' :: Int -> Writer (LogStream (Show :&&: Unit)) Int
-}
-- but this doesn't really give implication. We can almost get
-- implication for constraints built from the combinators with
class (c1 :: * -> Constraint) :=>: (c2 :: * -> Constraint)
instance c :=>: c
instance (c1 :=>: c) => (c1 :&&: c2) :=>: c
{-
instance (c2 :=>: c) => (c1 :&&: c2) :=>: c
-}
-- except the last instance overlaps with the previous one, so we
-- might need something like instance chains here.

----------------------------------------------------------------

deriving instance Show (LogTree ((~) String))
deriving instance Show (LogEvent ((~) String))

instance Signature String where
  type Arg String = String
  type Ret String = String

f , f' :: Int -> MaybeT (Writer (LogStream ((~) String))) String
f n = do
  tell [BeginCall "f" (show n)]
  r <- f' n
  tell [EndCall "f" r]
  return r
f' 0 = return ""
f' 5 = do
  _ <- f 2
  fail ""
f' n = do
  r <- f (n - 1)
  return $ "X" ++ r

testStream :: Int -> LogStream ((~) String)
testStream = execWriter . runMaybeT . f

testForest :: Int -> Either ParseError (LogForest ((~) String))
testForest = stream2Forest . testStream

----------------------------------------------------------------

main = do
  print $ testStream 4
  print $ testForest 4
  putStrLn ""
  print $ testStream 6
  print $ testForest 6

----------------------------------------------------------------
