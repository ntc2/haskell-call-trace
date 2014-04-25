{-# OPTIONS_GHC -fno-warn-name-shadowing #-}

{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE DataKinds #-}

module Data.Function.Decorator.Process.UnixTree where

import Control.Exception.Base (assert)

import Data.Function.Decorator

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
unixTree (Ex2T t@(CallAndError {})) =
  layer' enter calls exit
  where
    (enter , exit) = callAndError t
    calls = map unixTree (_children' t ++
                          maybe [] (:[]) (_how t))

layer' :: [String] -> [[String]] -> [String] -> [String]
layer' = layer ("─ " , "┬ " , "├ " , "└ ")

               ("├─" , "│ " , "└─" , "  ")

               ("╞ " , "╘ ")

----------------------------------------------------------------

-- Prefix first line with 'first', intermediate lines with 'between',
-- and last line with 'last''. If there is only one line, then prefix
-- with 'firstAndOnly' instead.
prefix :: String -> String -> String -> String -> [String] -> [String]
prefix _ _ _ _ [] = []
prefix firstAndOnly _ _ _ [l] = [firstAndOnly ++ l]
prefix _ first between last' (l:ls) =
  [first ++ l] ++
  map (between ++) (init ls) ++
  [last' ++ last ls]

-- Layer a header 'hs', body 'bss', and footer 'fs'.
--
-- The 'bLastFirst' and 'bLastRest' are used only if the footer is
-- empty; otherwise, 'bFirst' and 'bRest' are used for all body
-- lines.
--
-- The 'hFirstAndNoMore' and 'hLastAndNoMore' are used when the body
-- and footer are empty: the 'hFirstAndNoMore' is used when the header
-- consists of a single line, and the 'hLastAndNoMore' is used for the
-- last line of the header otherwise.
--
-- Assumes the header is non-empty.
layer :: (String , String , String , String) ->
         (String , String , String , String) ->
         (String , String) ->
         [String] -> [[String]] -> [String] -> [String]
layer (hFirstAndNoMore , hFirst , hRest , hLastAndNoMore)
      (bFirst , bRest , bLastFirst , bLastRest)
      (fInit , fLast) hs bss fs =
  hs' ++ concat bss' ++ fs'
  where
    hs' = assert (not . null $ hs) $
      if null fs && null bss
      then prefix hFirstAndNoMore hFirst hRest hLastAndNoMore hs
      else prefix hFirst          hFirst hRest hRest          hs
    fs' = prefix fLast fInit fInit fLast fs
    bss' = case bss of
      [] -> []
      _  ->
        if null fs
        -- Empty footer: prefix last group of body lines specially.
        then map (prefix bFirst bFirst bRest bRest) (init bss) ++
             [prefix bLastFirst bLastFirst bLastRest bLastRest (last bss)]
        -- Non-empty footer: prefix all body groups equally.
        else map (prefix bFirst bFirst bRest bRest) bss

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
