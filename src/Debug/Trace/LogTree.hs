{-# LANGUAGE KindSignatures
           , TypeFamilies
           , ExistentialQuantification
           , ConstraintKinds
           #-}

module Debug.Trace.LogTree where

import GHC.Prim (Constraint)

----------------------------------------------------------------

class Signature call where
  type Arg call
  type Ret call

type SigWith (c :: * -> Constraint) call = (Signature call , c call)

data LogEvent (c :: * -> Constraint)
  = forall call. SigWith c call => BeginCall call (Arg call)
  | forall call. SigWith c call => EndCall   call (Ret call)
type LogStream c = [LogEvent c]

data LogTree (c :: * -> Constraint)
  = forall call. SigWith c call =>
    CallAndReturn call (Arg call) [LogTree c] (Ret call)
  | forall call. SigWith c call =>
    CallAndError  call (Arg call) [LogTree c] (Maybe (LogTree c))

----------------------------------------------------------------
