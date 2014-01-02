{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE DataKinds #-}

module Debug.Trace.LogTree.Process.UnixTree where

import Debug.Trace.LogTree

class UnixTree call where
  callAndReturn :: LogTree UnixTree call "CallAndReturn" ->
    ([String] , [String])
  callAndError  :: LogTree UnixTree call "CallAndError" ->
    ([String] , [String])

unixTree :: Ex2T (LogTree UnixTree) -> [String]
unixTree (Ex2T t@(CallAndReturn {})) =
  [(enter !! 0) ++ " = " ++ (exit !! 0)] ++ map ('\t':) calls
  where
    (enter , exit) = callAndReturn t
    calls = concatMap unixTree (_children t)
unixTree (Ex2T t@(CallAndError {})) =
  [(enter !! 0) ++ " = " ++ (err !! 0)] ++ map ('\t':) calls
  where
    (enter , err) = callAndError t
    calls = concatMap unixTree (_children' t ++
                                maybe [] (:[]) (_how t))

-- Fancy line drawing characters copied from the 'tree' program. File
-- color.c in version 1.6.0, from
-- http://mama.indstate.edu/users/ice/tree/. I believe I can replace
-- the "\302\240" by spaces in the first command, and can probably
-- embed the fancy chars directly in the haskell program.  Use 'print'
-- in the shell to convert.
{-
{ utf8,        "\342\224\202\302\240\302\240",
    "\342\224\234\342\224\200\342\224\200", "\342\224\224\342\224\200\342\224\200", "\302\251" },
{ viscii,      "|  ",              "|--",            "`--",            "\371"     },
-}
