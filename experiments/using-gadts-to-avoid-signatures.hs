{-# OPTIONS_GHC -Wall #-}

{-# LANGUAGE DataKinds #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE ConstraintKinds #-}
{-# LANGUAGE PolyKinds #-}  -- For 'Ex'.

-- Proof of concept for using GADT indexes to avoid having to repeat
-- cumbersome signatures when defining classes that act on
-- heterogeneous lists satisfying a common constraint.  Here there are
-- no classes, but 't1' and 't2' correspond to the class functions:
-- note that they have simple signatures: they take 'T's of a
-- particular constructor, specified by the index.  GHC is smart
-- enough to check the exhaustiveness here.

data T c i where
  T1 :: c a => a -> Bool -> Ts c -> T c "T1"
  T2 :: c a => Bool -> a -> Ts c -> T c "T2"
type Ts c = [Ex (T c)]

data Ex p where
  Ex :: p a -> Ex p

t1 :: T c "T1" -> Bool
t1 (T1 _ b _) = b
t2 :: T c "T2" -> Bool
t2 (T2 b _ _) = b

f :: Ts Show -> [(String , Bool)]
f [] = []
f (et:ts') = case et of
  Ex t@(T1 x _ ts'') -> (show x , t1 t) : f (ts'' ++ ts')
  Ex t@(T2 _ x ts'') -> (show x , t2 t) : f (ts'' ++ ts')

ts :: Ts Show
ts = [Ex $ T1 (3::Int) True (tail ts) , Ex $ T2 False (4::Int) [head ts]]

main :: IO ()
main = print (take 10 $ f ts)

----------------------------------------------------------------
-- Motivating example.

{-
diff --git src/Debug/Trace/LogTree.hs src/Debug/Trace/LogTree.hs
index ee6bc7b..2f2cf51 100644
--- src/Debug/Trace/LogTree.hs
+++ src/Debug/Trace/LogTree.hs
@@ -2,7 +2,7 @@
 {-# LANGUAGE TypeFamilies #-}
 {-# LANGUAGE ExistentialQuantification #-}
 {-# LANGUAGE ConstraintKinds #-}
-
+{-# LANGUAGE GADTs #-}
 
 module Debug.Trace.LogTree where
 
@@ -31,6 +31,26 @@ data LogEvent (c :: * -> Constraint)
     EndCall   call (Before call) (Arg call) (Ret call) (After call)
 type LogStream c = [LogEvent c]
 
+-- XXX: why can't I use type synonyms for term constructors or class
+-- functions?  Type synonyms for plain functions work just fine.
+type CallAndReturnTy c call r =
+  call -> Before call -> Arg call -> LogForest c -> Ret call -> After call -> r
+
+type CallAndErrorTy c call r =
+  call -> Before call -> Arg call -> LogForest c -> Maybe (LogTree c) -> r
+
+{-
+data LogTree (c :: * -> Constraint) where
+  CallAndReturn :: SigWith c call => CallAndReturnTy c call (LogTree c)
+  CallAndError  :: SigWith c call => CallAndErrorTy  c call (LogTree c)
+
+    Data constructor `CallAndReturn' returns type `CallAndReturnTy
+                                                     c call (LogTree c)'
+      instead of an instance of its parent type `LogTree c'
+    In the definition of data constructor `CallAndReturn'
+    In the data declaration for `LogTree'
+-}
+
 data LogTree (c :: * -> Constraint)
   = forall call. SigWith c call =>
     CallAndReturn call (Before call) (Arg call) (LogForest c) (Ret call) (After call)
-}