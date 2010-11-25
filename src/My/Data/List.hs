--------------------------------------------------------------------
-- |
-- Module    :  My.Data.List
-- Copyright :  2009 (c) Dmitry Antonyuk
-- License   :  MIT
--
-- Maintainer:  Dmitry Antonyuk <lomeo.nuke@gmail.com>
-- Stability :  experimental
-- Portability: portable
--
-- List related utilities.
--
--------------------------------------------------------------------

module My.Data.List (
    split,          -- (Eq a) => a -> [a] -> [[a]]
    splitBy         -- (a -> Bool) -> [a] -> [[a]]
) where

import Data.List (unfoldr)

import My.Data.Tuple (onSnd)

-- |Splits list by delimiter element.
--
-- Inspired by Olivier's implementation of split:
-- <http://julipedia.blogspot.com/2006/08/split-function-in-haskell.html>
--
-- See comments.
splitBy :: (a -> Bool) -> [a] -> [[a]]
splitBy = unfoldr . go
    where
        go _ [] = Nothing
        go p xs = Just $ onSnd (drop 1) $ span p xs

split :: (Eq a) => a -> [a] -> [[a]]
split d = splitBy (/=d)

