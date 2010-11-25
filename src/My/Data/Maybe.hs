--------------------------------------------------------------------
-- |
-- Module    :  My.Data.Maybe
-- Copyright :  2009 (c) Dmitry Antonyuk
-- License   :  MIT
--
-- Maintainer:  Dmitry Antonyuk <lomeo.nuke@gmail.com>
-- Stability :  experimental
-- Portability: portable
--
-- 'Maybe' related utilities.
--
--------------------------------------------------------------------

module My.Data.Maybe where

import Control.Applicative (Applicative, pure, (<$>))

boolToMaybe, (?) :: Bool -> a -> Maybe a

boolToMaybe True x = Just x
boolToMaybe _    _ = Nothing

(?) = boolToMaybe

boolToMaybeM ::  (Applicative f) => Bool -> f a -> f (Maybe a)
boolToMaybeM True m = Just <$> m
boolToMaybeM _    _ = pure Nothing

tryMaybe :: IO a -> IO (Maybe a)
tryMaybe act = fmap Just act `catch` (\_ -> return Nothing)
