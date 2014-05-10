-- See
-- http://stackoverflow.com/questions/3079537/orphaned-instances-in-haskell
{-# OPTIONS_GHC -fno-warn-orphans #-}
{-# OPTIONS_GHC -fno-warn-name-shadowing #-}

-- GHC extensions: collect them all!
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
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE ScopedTypeVariables #-}
-- For 'processDefault's 'Proxy' argument.
{-# LANGUAGE PolyKinds #-}
{-# LANGUAGE TemplateHaskell #-}

module Data.Function.Decorator.Test where

import Prelude hiding (log)

import Control.Monad.Trans.Maybe
import Control.Monad.Writer
import Data.IORef
import Data.Proxy
import Text.Parsec hiding (State)

import Data.Function.Decorator.ConstraintLogic
import Data.Function.Decorator.Logger.HetCall
import Data.Function.Decorator.Logger.Logger
import Data.Function.Decorator.Logger.LogTree
import Data.Function.Decorator.Logger.Processor.UnixTree
import Data.Function.Decorator.Logger.SimpleCall
import Data.Function.Decorator.Logger.Unsafe

----------------------------------------------------------------
-- Isolate imports for memoizer tests, in case I want to factor them
-- into a second test file.

import Control.Applicative
import Control.Monad.State
import qualified Data.Map.Strict as Map
import Data.Typeable
import Text.Printf

import Data.Function.Decorator.Curry hiding (Args , Ret)
import Data.Function.Decorator.Memoizer
import Data.Function.Decorator.Memoizer.Unsafe

----------------------------------------------------------------
-- Logging tests.
----------------------------------------------------------------

deriving instance Show (LogTree AllShow call name)
deriving instance Show (Ex2T (LogTree AllShow))
deriving instance Show (LogEvent AllShow)

instance Signature String where
  name = id
  type Before String = String
  type Args String = String
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

class    (Signature t , Show t , Show (Before t) , Show (Args t) , Show (Ret t) , Show (After t))
  => AllShow t
instance (Signature t , Show t , Show (Before t) , Show (Args t) , Show (Ret t) , Show (After t))
  => AllShow t

fSimple :: FTy
fSimple = log (Proxy::Proxy "g") (return ()) (return ()) (f' fSimple)

----------------------------------------------------------------
-- Auto logging with unix tree post processor.

type C = UnixTree :&&: SigAll Show
type M = MaybeT (Writer (LogStream C))

type FTy' = Int -> M String
fSimple' :: FTy'
f'' :: FTy'
fSimple' = log (Proxy::Proxy "f") (return ()) (return ()) f''
f'' 0 = return " Y"
f'' 5 = do
  _ <- fSimple' 2
  fail ""
f'' n = do
  r <- fSimple' (n - 1)
  s <- gSimple "X" r
  return s

type GTy = String -> String -> M String
gSimple , g'' :: GTy
gSimple = log (Proxy::Proxy "g") (return ()) (return ()) g''
g'' s1 s2 = return $ s1 ++ s2

type HTy = Maybe Int -> M ()
hSimple , h'' :: HTy
hSimple = log (Proxy::Proxy "h") (return ()) (return ()) h''
h'' (Just n) = do
  _ <- fSimple' n
  _ <- fSimple' n
  return ()
h'' Nothing = fail "Nothing"

----------------------------------------------------------------

-- Nice: if you forget an instance the error message tells you exactly
-- what the sig is of course :D
instance UnixTree (SimpleCall "f" () FTy' ()) where
  callAndReturn t =
    ([formatCall "f" $ _arg t] , [show ret])
    where
      ret = _ret t
  callAndError t =
    ([formatCall "f" $ _arg' t] , [maybe "<error: here>" (const "<error: there>") how])
    where
      how = _how t

instance UnixTree (SimpleCall "g" () GTy ()) where
  callAndReturn (CallAndReturn {..}) =
    ( [ show _before
      , hfoldl (Proxy::Proxy Show) (\s x -> s ++ " " ++ show x) "g" _arg]
    , [ show _ret , show _after ] )
  callAndError (CallAndError {..}) =
    ([formatCall (name _call') _arg'] , [maybe "<error: here>" (const "<error: there>") _how])

instance UnixTree (SimpleCall "h" () HTy ()) where
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

----------------------------------------------------------------
-- Generic 'UnixTree' processors for trees whose parts satisfy 'Show'.

-- An instance for 'Show' that brackets the subcalls between the
-- before state and args and the return value and after state.
instance UnixTree (HetCall Show "default:bracket") where
  callAndReturn (CallAndReturn {..}) =
    ( [ unH show _before
      , formatCall (name _call) _arg ]
    , [ unH show _ret
      , unH show _after ] )
  callAndError (CallAndError {..}) =
    ( [ unH show _before'
      , formatCall (name _call') _arg' ]
    , [ maybe "<error: here>" (const "<error: there>") _how ] )

-- An instance for 'Show' that puts everything (before, args, return,
-- after) in the header.
instance UnixTree (HetCall Show "default:header") where
  callAndReturn (CallAndReturn {..}) =
    ( [ unH show _before
      , formatCall (name _call) _arg ++ " = " ++ unH show _ret
      , unH show _after ]
    , [] )
  callAndError (CallAndError {..}) =
    ( [ unH show _before'
      , formatCall (name _call') _arg' ++ " = " ++
          maybe "<error: here>" (const "<error: there>") _how ]
    , [] )

-- An instance for 'Show' that puts everything (before, args, return,
-- after) in a single line in the header.
instance UnixTree (HetCall Show "default:oneline") where
  callAndReturn (CallAndReturn {..}) =
    ( [ "<" ++ unH show _before ++ "> " ++
        formatCall (name _call) _arg ++ " = " ++ unH show _ret ++
        " <" ++ unH show _after ++ ">" ]
    , [] )
  callAndError (CallAndError {..}) =
    ( [ "<" ++ unH show _before' ++ "> " ++
        formatCall (name _call') _arg' ++ " = " ++
          maybe "<error: here>" (const "<error: there>") _how ]
    , [] )

----------------------------------------------------------------
-- Display the results.

testUnixTree :: (LogForest C -> String) -> (a -> M b) -> a -> String
testUnixTree process f =
  either show process .
  stream2Forest .
  execWriter .
  runMaybeT .
  f

processCustom :: LogForest C -> String
processCustom =
  unlines .
  unixTree .
  coerceLogTree' (\x -> x) .
  head

processDefault :: UnixTree (HetCall Show tag) => Proxy tag -> LogForest C -> String
processDefault p =
  unlines .
  unixTree .
  heterogenize (Proxy::Proxy Show) p .
  coerceLogTree' (\x -> x) .
  head

----------------------------------------------------------------
-- Parser tests.

testStream :: FTy -> Int -> LogStream AllShow
testStream f = execWriter . runMaybeT . f

testForest :: FTy -> Int -> Either ParseError (LogForest AllShow)
testForest f = stream2Forest . testStream f

----------------------------------------------------------------
-- Logger test to run in 'main'.

logMain :: IO ()
logMain = do
  forM_ [ ("fManual" , fManual)
        , ("fSimple" , fSimple)
        ] $ \(s , f) -> do
    putStrLn $ "Stream Test: " ++ s
    putStrLn "================================================================"
    print $ testStream f 4
    print $ testForest f 4
    putStrLn ""
    print $ testStream f 6
    print $ testForest f 6
    putStrLn ""
    putStrLn ""

  forM_ [ ("processCustom" , processCustom)
        , ("processDefault: bracket" , processDefault (Proxy::Proxy "default:bracket"))
        , ("processDefault: header" , processDefault (Proxy::Proxy "default:header"))
        , ("processDefault: oneline" , processDefault (Proxy::Proxy "default:oneline"))
        ] $ \(s , p) -> do
    putStrLn $ "Processor Test: " ++ s
    putStrLn "================================================================"
    putStrLn $ testUnixTree p fSimple' 4
    putStrLn ""
    putStrLn $ testUnixTree p fSimple' 6
    putStrLn ""
    putStrLn $ testUnixTree p hSimple (Just 4)
    putStrLn ""
    putStrLn $ testUnixTree p hSimple (Just 6)
    putStrLn ""
    putStrLn $ testUnixTree p hSimple Nothing
    putStrLn ""
    putStrLn ""

----------------------------------------------------------------
-- Memoizer tests.
----------------------------------------------------------------

type MemoM = StateT S IO

data S = S { _fibDict :: Map.Map (ArgsM FibTy) (RetM FibTy)
           , _hDict :: Map.Map String (H Typeable)
           , _indentRef :: IORef Int
           }

type FibTy = Integer -> MemoM Integer
fib , fib' :: FibTy
fib = simpleMemoize lookup insert fib' where
  lookup k = Map.lookup k <$> gets _fibDict
  insert k v = modify i where
    i s = s { _fibDict = Map.insert k v $ _fibDict s }
fib' n =
  if n <= 1
  then pure n
  else (+) <$> fib (n-1) <*> fib (n-2)

-- This looks like the same boilerplate as in 'fib' above, except here
-- the insert and lookup functions are the *same* for all memoized
-- functions, and so can be factored out and defined only once.
--
-- GHC infers a too-special type here, so we can't use this function
-- twice.
{-
specializedCastMemoizer = castMemoize lookup insert where
  lookup k = Map.lookup k <$> gets _hDict
  insert k v = modify i where
    i s = s { _hDict = Map.insert k v $ _hDict s }
-}
lookupM ::
  forall (f :: * -> *).
  (Functor f, MonadState S f) =>
  String -> f (Maybe (H Typeable))
lookupM k = Map.lookup k <$> gets _hDict
insertM ::
  forall (m :: * -> *).
  MonadState S m =>
  String -> H Typeable -> m ()
insertM k v = modify i where
  i s = s { _hDict = Map.insert k v $ _hDict s }


fib2 :: FibTy
fib2 = castMemoize lookupM insertM "Test.fib2" fib' where
  fib' n =
    if n <= 1
    then pure n
    else (+) <$> fib2 (n-1) <*> fib2 (n-2)

pow :: Int -> Integer -> MemoM Integer
pow = castMemoize lookupM insertM "Test.pow" pow' where
  pow' _ 0 = pure 1
  pow' n p = sum <$> replicateM n (pow n (p-1))

----------------------------------------------------------------
-- Open recursion.

openFib :: FibTy -> FibTy
openFib fib n =
  if n <= 1
  then pure n
  else (+) <$> fib (n-1) <*> fib (n-2)

-- We can use a decorator designed for a mutual recursive definitions
-- with an open recursive function. Instead of
--
--   f  = decorate f'
--   f' = ... f ...
--
-- we have
--
--   f       = fix (decorate . openF)
--   openF f = ... f ...
fib3 :: FibTy
fib3 = fix (castMemoize lookupM insertM "Test.fib3" . openFib)

----------------------------------------------------------------
-- Printf logging / tracing.

getIndentRef :: MemoM (IORef Int)
getIndentRef = gets _indentRef

tracedFib :: FibTy
tracedFib =
  fix $ trace getIndentRef "tFib"
      . openFib

tracedMemoizedFib :: FibTy
tracedMemoizedFib =
  fix $ trace getIndentRef "tmFib"
      . castMemoize lookupM insertM "Test.tmFib"
      . openFib

----------------------------------------------------------------

testMemo :: MemoM a -> IO a
testMemo ma = do
  indentRef <- newIORef 0
  evalStateT ma (S { _fibDict = Map.empty
                   , _hDict = Map.empty
                   , _indentRef = indentRef })

memoMain :: IO ()
memoMain = do
  putStrLn "Fib Test"
  putStrLn "================================================================"

  forM_ [0,10..300] $ \ n -> do
    _ <- printf "fib   %3i = %i\n" n =<< (testMemo $ fib n)
    _ <- printf "fib2  %3i = %i\n" n =<< (testMemo $ fib2 n)
    _ <- printf "fib3  %3i = %i\n" n =<< (testMemo $ fib3 n)
    _ <- printf "pow 2 %3i = %i\n" n =<< (testMemo $ pow 2 n)
    return ()

  putStrLn ""
  putStrLn "Traced"
  putStrLn "----------------------------------------------------------------"
  _ <- testMemo (tracedFib 4)

  putStrLn ""
  putStrLn "Traced and Memoized"
  putStrLn "----------------------------------------------------------------"
  _ <- testMemo (tracedMemoizedFib 4 >> liftIO (putStrLn "") >> tracedMemoizedFib 4)

  return ()

----------------------------------------------------------------
-- Test unsafe functions.

type PureFibTy = Integer -> Integer

openPureFib :: PureFibTy -> PureFibTy
openPureFib fib n =
  if n <= 1
  then n
  else fib (n-1) + fib (n-2)

tracedPureFib :: PureFibTy
tracedPureFib =
  fix (unsafeTrace (Proxy::Proxy $(nat 1)) "tpFib" . openPureFib)

memoizedPureFib :: PureFibTy
memoizedPureFib =
  fix (unsafeMemoize $(proxyNat 1) . openPureFib)

tracedMemoizedPureFib :: PureFibTy
tracedMemoizedPureFib =
  fix $ unsafeTrace   (Proxy::Proxy $(nat 1)) "tmpFib" .
        unsafeMemoize (Proxy::Proxy $(nat 1)) .
        openPureFib

unsafeMain :: IO ()
unsafeMain = do
  putStrLn ""
  putStrLn "Unsafe Traced"
  putStrLn "----------------------------------------------------------------"
  _ <- printf "(tracedPureFib 4 = %i)\n" (tracedPureFib 4)
  return ()

  putStrLn ""
  putStrLn "Unsafe Memoized"
  putStrLn "----------------------------------------------------------------"
  _ <- printf "(memoizedPureFib 300 = %i)\n" (memoizedPureFib 300)
  return ()

  putStrLn ""
  putStrLn "Unsafe Traced Memoized"
  putStrLn "----------------------------------------------------------------"
  _ <- printf "(tracedMemoizedPureFib 30 = %i)\n" (tracedMemoizedPureFib 30)
  return ()


----------------------------------------------------------------

main :: IO ()
main = do
  logMain
  memoMain
  unsafeMain
