--------------------------------------------------------------------
-- |
-- Module    :  My.Data.Tuple
-- Copyright :  2009 (c) Dmitry Antonyuk
-- License   :  MIT
--
-- Maintainer:  Dmitry Antonyuk <lomeo.nuke@gmail.com>
-- Stability :  experimental
-- Portability: portable
--
-- @Tuple@ related utilities.
--
--------------------------------------------------------------------

module My.Data.Tuple (
    Fst(..),
    Snd(..),
    mapPair,
    onFst,
    onSnd
) where

newtype Fst b a = Fst {
    fromFst :: (a, b)
}

newtype Snd a b = Snd {
    fromSnd :: (a, b)
}

instance Functor (Fst a) where
    fmap f (Fst (x, y)) = Fst (f x, y)

instance Functor (Snd a) where
    fmap f (Snd (x, y)) = Snd (x, f y)

mapPair ::  (a -> c, b -> d) -> (a, b) -> (c, d)
mapPair (f, g) (x, y) = (f x, g y)

onFst ::  (a -> c) -> (a, b) -> (c, b)
onFst f (x, y) = (f x, y)

onSnd ::  (b -> c) -> (a, b) -> (a, c)
onSnd f (x, y) = (x, f y)

