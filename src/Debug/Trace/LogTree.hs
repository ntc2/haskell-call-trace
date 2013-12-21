{-# LANGUAGE KindSignatures
           , TypeFamilies
           , ExistentialQuantification
           , ConstraintKinds
           #-}

module Debug.Trace.LogTree where

import GHC.Prim (Constraint)

import Control.Applicative ((<*), (*>))
import Data.Typeable (cast , Typeable)
import Text.Parsec hiding (token , eof , anyToken)
import Unsafe.Coerce (unsafeCoerce)

----------------------------------------------------------------
-- Log types.

class Signature call where
  name :: call -> String
  type Arg call
  type Ret call

type SigWith (c :: * -> Constraint) call =
  (Signature call , Eq call , Typeable call , c call)

data LogEvent (c :: * -> Constraint)
  = forall call. SigWith c call => BeginCall call (Arg call)
  | forall call. SigWith c call => EndCall   call (Ret call)
type LogStream c = [LogEvent c]

data LogTree (c :: * -> Constraint)
  = forall call. SigWith c call =>
    CallAndReturn call (Arg call) (LogForest c) (Ret call)
  | forall call. SigWith c call =>
    CallAndError  call (Arg call) (LogForest c) (Maybe (LogTree c))
type LogForest c = [LogTree c]

----------------------------------------------------------------
-- Parsers from log streams into log trees.

stream2Forest :: LogStream c -> Either ParseError (LogForest c)
stream2Forest = parse (forest <* eof) "<no file>"

----------------------------------------------------------------

type P c a = Parsec (LogStream c) () a

forest :: P c (LogForest c)
forest = many tree

tree :: P c (LogTree c)
tree = try callAndReturn <|> callAndError

----------------------------------------------------------------

callAndReturn , callAndError :: P c (LogTree c)

callAndReturn = do
  BeginCall call arg <- beginCall
  children <- forest
  EndCall call' ret <- endCall
-- XXX: why can't GHC figure out that 'ret' has the right type when
-- 'call' and 'call'' are known to have the same type under the
-- 'cast'?  Surely type functions are known to be functions, and so
-- 'call ~ call'' will imply 'Ret call ~ Ret call''!
{-
  case cast call' of
    Just call' | call == call' ->
      return $ CallAndReturn call arg children ret
    _ ->
      fail "Inconsistent input stream: you logged a return without a corresponding call!"
-}
-- XXX: this also fails:
{-
  case (cast call', gcast ret) of
    (Just call' , Just ret) | call == call' ->
      return $ CallAndReturn call arg children ret
    _ ->
      fail "Inconsistent input stream: you logged a return without a corresponding call!"
-}
-- with "Could not deduce (Ret call ~ c0 b0)", so I guess a type
-- function name is not a type constructor for the purposes of
-- 'gcast'.  Maybe: if we think of the type function call 'Ret call'
-- as already reduced, than certainly there's no way to know what form
-- it takes.
  case cast call' of
    Just call' | call == call' ->
      return $ CallAndReturn call arg children (unsafeCoerce ret)
    _ ->
      fail "Inconsistent input stream: you logged a return without a corresponding call!"

callAndError = do
  BeginCall call arg <- beginCall
  children <- forest <* eof
  return $
    -- May be more understandable to have two different error nodes,
    -- e.g. 'CallAndErrorHere' and 'CallAndErrorThere', and then we
    -- wouldn't use 'Maybe' in the last arg to distinguish the two
    -- error types.
    --
    -- The idea now is that the last arg is 'Nothing' if the error
    -- happened here, in the current call, and it's 'Just c' if the
    -- error was already present in the last child call 'c'.
    case last children of
      l | not (null children) , CallAndError {} <- l ->
        CallAndError call arg (init children) (Just l)
      _ ->
        CallAndError call arg children Nothing

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
