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
{-# LANGUAGE GADTs #-}
{-# LANGUAGE RecordWildCards #-}

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

deriving instance Show (LogTree AllShow call name)
deriving instance Show (Ex2T (LogTree AllShow))
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

class    (Signature t , Show t , Show (Before t) , Show (Arg t) , Show (Ret t) , Show (After t))
  => AllShow t
instance (Signature t , Show t , Show (Before t) , Show (Arg t) , Show (Ret t) , Show (After t))
  => AllShow t

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
  s <- gSimple "X" r
  return s

type GTy = String -> String -> MaybeT (Writer (LogStream UnixTree)) String
gSimple , g'' :: GTy
gSimple = simpleLogger (Proxy::Proxy "g") (return ()) (return ()) g''
g'' s1 s2 = return $ s1 ++ s2

type HTy = Int -> MaybeT (Writer (LogStream UnixTree)) ()
hSimple , h'' :: HTy
hSimple = simpleLogger (Proxy::Proxy "h") (return ()) (return ()) h''
h'' n = do
  _ <- fSimple' n
  _ <- fSimple' n
  return ()

-- Nice: if you forget an instance the error message tells you exactly
-- what the sig is of course :D
instance UnixTree (Proxy (SimpleCall "f" () FTy' ())) where
  callAndReturn t =
    (["f " ++ show arg] , [show ret])
    where
      (arg,()) = _arg t
      ret = _ret t
  callAndError t =
    (["f " ++ show arg] , [maybe "<error: here>" (const "<error: there>") how])
    where
      (arg,()) = _arg' t
      how = _how t

instance UnixTree (Proxy (SimpleCall "g" () GTy ())) where
  callAndReturn (CallAndReturn {..}) =
    ([show _before , "g " ++ show s1 ++ " " ++ show s2] , [show _ret , show _after])
    where
      (s2,(s1,())) = _arg
  callAndError (CallAndError {..}) =
    (["g " ++ show s1 ++ " " ++ show s2] , [maybe "<error: here>" (const "<error: there>") _how])
    where
      (s2,(s1,())) = _arg'

instance UnixTree (Proxy (SimpleCall "h" () HTy ())) where
  callAndReturn (CallAndReturn {..}) =
    ([show _before , "h " ++ show n ++ " = " ++ show _ret , show _after] , [])
    where
      (n,()) = _arg
  callAndError (CallAndError {..}) =
    ( [ show _before'
      , "h " ++ show n ++ " = " ++
        maybe "<error: here>" (const "<error: there>") _how ]
    , [] )
    where
      (n,()) = _arg'

testUnixTree :: (a -> MaybeT (Writer (LogStream UnixTree)) b)
             -> a -> String
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
  putStrLn ""
  putStrLn $ testUnixTree hSimple 4
  putStrLn ""
  putStrLn $ testUnixTree hSimple 6


----------------------------------------------------------------
