-- See
-- http://stackoverflow.com/questions/3079537/orphaned-instances-in-haskell
{-# OPTIONS_GHC -fno-warn-orphans #-}

{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE ConstraintKinds #-}
{-# LANGUAGE UndecidableInstances #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleContexts #-}

module Debug.Trace.LogTree.Test where

import Control.Monad.Trans.Maybe
import Control.Monad.Writer
import Data.Proxy
import Text.Parsec

import Debug.Trace.LogTree
import Debug.Trace.LogTree.Process.UnixTree
import Debug.Trace.LogTree.Simple.Logger
import Debug.Trace.LogTree.Simple.Call

----------------------------------------------------------------

deriving instance Show (LogTree AllShow)
deriving instance Show (LogEvent AllShow)

instance Signature String where
  name = id
  type Before String = String
  type Arg String = String
  type Ret String = String
  type After String = String

----------------------------------------------------------------
-- Manual logging example.

type FTy = Int -> MaybeT (Writer (LogStream AllShow)) String

fManual :: FTy
f' :: FTy -> FTy
fManual n = do
  tell [BeginCall "f" "" (show n)]
  r <- f' fManual n
  tell [EndCall "f" "" (show n) r ""]
  return r
f' _ 0 = return ""
f' f 5 = do
  _ <- f 2
  fail ""
f' f n = do
  r <- f (n - 1)
  return $ "X" ++ r

----------------------------------------------------------------
-- Auto logging example.

class (Signature t , Show t , Show (Before t) , Show (Arg t) , Show (Ret t) , Show (After t)) => AllShow t
instance (Signature t , Show t , Show (Before t) , Show (Arg t) , Show (Ret t) , Show (After t)) => AllShow t

fSimple :: FTy
fSimple = simpleLogger (Proxy::Proxy "g") (return ()) (return ()) (f' fSimple)

----------------------------------------------------------------
-- Auto logging with unix tree post processor.

-- instance (SingI tag , UncurryM sig)
--       => Signature (

-- type MkSimpleCall (tag::Symbol) before after t = SimpleCall

type FTy' = Int -> MaybeT (Writer (LogStream UnixTree)) String

fSimple' :: FTy'
f'' :: FTy'
fSimple' = simpleLogger (Proxy::Proxy "f") (return ()) (return ()) f''
f'' 0 = return ""
f'' 5 = do
  _ <- fSimple' 2
  fail ""
f'' n = do
  r <- fSimple' (n - 1)
  return $ "X" ++ r

-- Nice: if you forget an instance the error message tells you exactly
-- what the sig is of course :D
instance UnixTree (Proxy (SimpleCall "f" () FTy' ())) where
  callAndReturn' _ _ (arg,()) _ ret _ =
    (["f " ++ show arg] , [show ret])
  callAndError'  _ _ (arg,()) _ how =
    (["f " ++ show arg] , [maybe "<error: here>" (const "<error: there>") how])

testUnixTree :: FTy' -> Int -> String
testUnixTree f = either show (unlines . unixTree . head)
               . stream2Forest
               . execWriter
               . runMaybeT . f

----------------------------------------------------------------

testStream :: FTy -> Int -> LogStream AllShow
testStream f = execWriter . runMaybeT . f

testForest :: FTy -> Int -> Either ParseError (LogForest AllShow)
testForest f = stream2Forest . testStream f

----------------------------------------------------------------

main :: IO ()
main = do
  forM_ [fManual , fSimple] $ \f -> do
    print $ testStream f 4
    print $ testForest f 4
    putStrLn ""
    print $ testStream f 6
    print $ testForest f 6
    putStrLn ""
    putStrLn ""

  putStrLn $ testUnixTree fSimple' 4
  putStrLn ""
  putStrLn $ testUnixTree fSimple' 6

----------------------------------------------------------------
