{-# LANGUAGE KindSignatures #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE ExistentialQuantification #-}
{-# LANGUAGE ConstraintKinds #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE PolyKinds #-}

module Debug.Trace.LogTree
  ( Ex2T(..)
  , Signature(..)
  , LogEvent(..)
  , LogStream
  , LogTree(..)
  , LogForest
  , stream2Forest
  )
where

import GHC.Prim (Constraint)
import GHC.TypeLits (Symbol)

import Control.Applicative ((<*), (*>))
import Text.Parsec hiding (token , eof , anyToken)

-- XXX: move this somewhere else
--
-- A type family with two existentially quantified type indices.
data Ex2T f where
  Ex2T :: f a b -> Ex2T f

----------------------------------------------------------------
-- Log types.

class Signature call where
  name :: call -> String
  type Before call
  type Arg call
  type Ret call
  type After call

type SigWith (c :: * -> Constraint) call =
  (Signature call , c call)

data LogEvent (c :: * -> Constraint)
  = forall call. SigWith c call =>
    BeginCall call (Before call) (Arg call)
  | forall call. SigWith c call =>
    EndCall   call (Before call) (Arg call) (Ret call) (After call)
type LogStream c = [LogEvent c]

-- Would like to put the constraint on the whole data type, since we
-- have it for all constructors, but apparently this is not compatible
-- with GADT syntax:
{-
{-# LANGUAGE DatatypeContexts #-}

data (Signature call , c call) => LogTree (c :: * -> Constraint) call (name :: Symbol) where

    No context is allowed on a GADT-style data declaration
    (You can put a context on each contructor, though.)
-}
-- But it gets better: since we can't have ad-hoc polymorphic field
-- names (patiently awaiting Gundry's record fix ...), we can't have
-- common projections (e.g. '_call') for both constructors, since they
-- have different 'con' indices.  So, prime the duplicated fields for
-- now :P
data LogTree (c :: * -> Constraint) call (name :: Symbol) where
  CallAndReturn :: SigWith c call =>
    { _call     :: call
    , _before   :: Before call
    , _arg      :: Arg call
    , _children :: LogForest c
    , _ret      :: Ret call
    , _after    :: After call
    } -> LogTree c call "CallAndReturn"
  CallAndError :: SigWith c call =>
    { _call'     :: call
    , _before'   :: Before call
    , _arg'      :: Arg call
    , _children' :: LogForest c
    , _how       :: Maybe (Ex2T (LogTree c))
    } -> LogTree c call "CallAndError"
type LogForest c = [Ex2T (LogTree c)]

----------------------------------------------------------------
-- Parsers from log streams into log trees.

stream2Forest :: LogStream c -> Either ParseError (LogForest c)
stream2Forest = parse (forest <* eof) "<no file>"

----------------------------------------------------------------

type P c a = Parsec (LogStream c) () a

forest :: P c (LogForest c)
forest = many tree

tree :: P c (Ex2T (LogTree c))
tree = try callAndReturn <|> callAndError

----------------------------------------------------------------

callAndReturn , callAndError :: P c (Ex2T (LogTree c))

callAndReturn = do
  BeginCall {} <- beginCall
  children <- forest
  EndCall call before arg ret after <- endCall
  return . Ex2T $ CallAndReturn call before arg children ret after

callAndError = do
  BeginCall call before arg <- beginCall
  children <- forest <* eof
  return . Ex2T $
    -- May be more understandable to have two different error nodes,
    -- e.g. 'CallAndErrorHere' and 'CallAndErrorThere', and then we
    -- wouldn't use 'Maybe' in the last arg to distinguish the two
    -- error types.
    --
    -- The idea now is that the last arg is 'Nothing' if the error
    -- happened here, in the current call, and it's 'Just c' if the
    -- error was already present in the last child call 'c'.  This
    -- gives easy reconstruction of a inward stack trace: keep
    -- following the 'Just's until you arrive at the frame the error
    -- originated from.  May be useful to also have an easy stack
    -- trace in the success case ...
    case last children of
      l | not (null children) , Ex2T (CallAndError {}) <- l ->
        CallAndError call before arg (init children) (Just l)
      _ ->
        CallAndError call before arg children Nothing

----------------------------------------------------------------
-- Token parsers.

-- I would like to return '(call , Arg call)' here, but it seems that
-- doesn't work because 'call' would be existentially quantified.  The
-- following fully-quantified signature illustrates the problem:
--
--   beginCall :: forall c call. SigWith c call => P c (call , Arg call)
--
-- We can't return *any* type 'call' which satisfied 'SigWith c', but
-- rather, only the particular type 'call' which is hidden in the
-- current log-event token.
beginCall , endCall :: P c (LogEvent c)

-- XXX: could also use 'anyToken':
--
--   beginCall = do
--     t@(BeginCall {}) <- anyToken
--     return t
--
-- will the pattern match failure cause proper back tracking, or do I
-- need to wrap this?

beginCall = token "<BeginCall {}>" test where
  test t@(BeginCall {}) = Just t
  test _                = Nothing

endCall = token "<EndCall {}>" test where
  test t@(EndCall {}) = Just t
  test _              = Nothing

token :: String -> (LogEvent c -> Maybe a) -> P c a
token msg test = tokenPrim (const msg) update test where
  update p _ _ = p

----------------------------------------------------------------
-- Re-implementation of 'eof' and 'anyToken' that do not require
-- 'Show' of the tokens.
--
-- See
-- http://hackage.haskell.org/package/parsec-3.1.4/docs/src/Text-Parsec-Combinator.html#eof

eof :: Stream s m t => ParsecT s u m ()
eof = (try anyToken *> unexpected "token") <|> return ()

anyToken ::Stream s m t => ParsecT s u m t
anyToken = tokenPrim (const "<anyToken>") (\pos _ _ -> pos) Just

----------------------------------------------------------------
