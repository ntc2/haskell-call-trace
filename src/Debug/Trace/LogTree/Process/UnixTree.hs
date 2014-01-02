{-# OPTIONS_GHC -fno-warn-name-shadowing #-}

{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE DataKinds #-}

module Debug.Trace.LogTree.Process.UnixTree where

import Control.Exception.Base (assert)

import Debug.Trace.LogTree

----------------------------------------------------------------

-- The instances return header and footer lines for the call.  E.g.,
-- the header can include the initial state, function name, and
-- arguments, and the footer can include the return value and final
-- state.
class UnixTree call where
  callAndReturn :: LogTree UnixTree call "CallAndReturn" ->
    ([String] , [String])
  callAndError  :: LogTree UnixTree call "CallAndError" ->
    ([String] , [String])

unixTree :: Ex2T (LogTree UnixTree) -> [String]
unixTree (Ex2T t@(CallAndReturn {})) =
  layer' enter calls exit
  where
    (enter , exit) = callAndReturn t
    calls = map unixTree (_children t)
    layer' = layer ("┬ " , "├ ") ("├──" , "│  " , "└──" , "   ") ("╞ " , "╘ ")
unixTree (Ex2T t@(CallAndError {})) =
  layer' enter calls exit
  where
    (enter , exit) = callAndError t
    calls = map unixTree (_children' t ++
                          maybe [] (:[]) (_how t))
    layer' = layer ("┬ " , "├ ") ("├──" , "│  " , "└──" , "   ") ("╞ " , "╘ ")

----------------------------------------------------------------

-- Prefix first line with 'first' and rest of lines with 'rest'.
prefix :: String -> String -> [String] -> [String]
prefix _ _ [] = []
prefix first rest (l:ls) = (first ++ l) : map (rest ++) ls

-- Like 'prefix', but instead initial and last line are distinguished.
prefix' :: String -> String -> [String] -> [String]
prefix' init last = reverse . prefix last init . reverse

-- Layer a header 'hs', body 'bss', and footer 'fs'.
--
-- The 'bLastFirst' and 'bLastRest' are used only if the footer is
-- empty; otherwise, 'bFirst' and 'bRest' are used for all body lines.
--
-- Assumes the header is non-empty.
layer :: (String , String) -> (String , String , String , String) -> (String , String) ->
         [String] -> [[String]] -> [String] -> [String]
layer (hFirst , hRest) (bFirst , bRest , bLastFirst , bLastRest) (fInit , fLast) hs bss fs =
  hs' ++ concat bss' ++ fs'
  where
    hs' = assert (not . null $ hs) $ prefix hFirst hRest hs
    fs' = prefix' fInit fLast fs
    bss' = case bss of
      [] -> []
      _  ->
        if null fs
        -- Empty footer: prefix last group of body lines specially.
        then map (prefix bFirst bRest) (init bss) ++
             [prefix bLastFirst bLastRest (last bss)]
        -- Non-empty footer: prefix all body groups equally.
        else map (prefix bFirst bRest) bss

----------------------------------------------------------------

-- https://en.wikipedia.org/wiki/Box-drawing_character
--
-- The fancy line drawing characters in the 'tree' program are in file
-- color.c in version 1.6.0; see
-- http://mama.indstate.edu/users/ice/tree/.
{-
"┐"

"┬"

"│"

"├"

"╘"

"╞"

"└"

"─"

" "
-}

----------------------------------------------------------------
