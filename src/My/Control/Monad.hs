--------------------------------------------------------------------
-- |
-- Module    :  My.Control.Monad
-- Copyright :  2009 (c) Dmitry Antonyuk
-- License   :  MIT
--
-- Maintainer:  Dmitry Antonyuk <lomeo.nuke@gmail.com>
-- Stability :  experimental
-- Portability: portable
--
-- A lot of monadic helpers.
--
--------------------------------------------------------------------

module My.Control.Monad (
    -- * Unlift
    unliftM,    -- :: (Monad m) => (m a -> m r) -> (a -> m r)
    unliftM2,   -- :: (Monad m) => (Monad m) => (m a1 -> m a2 -> m r) -> (a1 -> a2 -> m r)
    unliftM3,   -- :: (Monad m) => (m a1 -> m a2 -> m a3 -> m r) -> (a1 -> a2 -> a3 -> m r)
    -- * Folding
    fold1M,     -- :: (Monad m) => (a -> a -> m a) -> [a] -> m a
    fold1M_,    -- :: (Monad m) => (a -> a -> m a) -> [a] -> m ()
    -- * Condition handling
    -- ** Boolean conditions
    whenM,      -- :: (Monad m) => m Bool -> m () -> m ()
    unlessM,    -- :: (Monad m) => m Bool -> m () -> m ()
    whileM,     -- :: (Monad m) => m Bool -> m () -> m ()
    untilM,     -- :: (Monad m) => m Bool -> m () -> m ()
    -- ** Maybe conditions
    whenJust,   -- :: (Monad m) => Maybe a -> (a -> m ()) -> m ()
    whenJustM,  -- :: (Monad m) => m (Maybe a) -> (a -> m ()) -> m ()
    whileJustM, -- :: (Monad m) => m (Maybe a) -> (a -> m ()) -> m ()
    -- * Binding forms
    (>>==),     -- :: (Monad m) => m a -> (a -> m b) -> m a
    (==<<)      -- :: (Monad m) => (a -> m b) -> m a -> m a
) where

import Control.Monad (foldM, foldM_, when, unless)

unliftM :: (Monad m) => (m a -> m r) -> (a -> m r)
unliftM f = f . return

unliftM2 :: (Monad m) => (m a1 -> m a2 -> m r) -> (a1 -> a2 -> m r)
unliftM2 f x1 x2 = f (return x1) (return x2)

unliftM3 :: (Monad m) => (m a1 -> m a2 -> m a3 -> m r) -> (a1 -> a2 -> a3 -> m r)
unliftM3 f x1 x2 x3 = f (return x1) (return x2) (return x3)

fold1M :: (Monad m) => (a -> a -> m a) -> [a] -> m a
fold1M _ []     = error "My.Control.Monad.fold1M: empty list"
fold1M f (x:xs) = foldM f x xs

fold1M_ :: (Monad m) => (a -> a -> m a) -> [a] -> m ()
fold1M_ _ []     = error "My.Control.Monad.fold1M_: empty list"
fold1M_ f (x:xs) = foldM_ f x xs

-- | Monadic analogue of when. For example,
--
-- >       whenM (isEOF f) (putStr "EOF reached.")
--
-- will output the string @EOF reached@ if isEOF f returns  'True',
-- and otherwise do nothing.
whenM :: (Monad m) => m Bool -> m () -> m ()
whenM p s = do
        b <- p
        when b s

unlessM :: (Monad m) => m Bool -> m () -> m ()
unlessM p s = do
        b <- p
        unless b s

{-
 - or unlessM p s = whenM (liftM not p) s
 -}

whileM ::  (Monad m) => m Bool -> m a -> m ()
whileM p s = loop
    where
        loop = whenM p $ s >> loop

untilM ::  (Monad m) => m Bool -> m a -> m ()
untilM p s = loop
    where
        loop = unlessM p $ s >> loop

-- | Execute monadic calculation if first argument is 'Just'.
--
-- Select value from 'Just' and pass it to monadic calculation.
-- If first argument is 'Nothing' then 'return' ().
whenJust :: (Monad m) => Maybe a -> (a -> m ()) -> m ()
whenJust Nothing  _ = return ()
whenJust (Just x) f = f x

-- | Monadic version of 'whenJust'.
--
-- Get value from monadic first argument, then pass it to monadic
-- calculation if it is 'Just'. Otherwise 'return' ().
whenJustM :: (Monad m) => m (Maybe a) -> (a -> m ()) -> m ()
whenJustM m f = do
    v <- m
    whenJust v f

-- | Monadic loop.
--
-- Run monadic calculation in cycle while monadic value from first
-- argument id 'Just'.
whileJustM :: (Monad m) => m (Maybe a) -> (a -> m ()) -> m ()
whileJustM m f = loop
    where
        loop = whenJustM m go
        go v = f v >> loop

-- | Do something with monadic value and return original value.
-- This function is useful in many stateful monads. E.g.
--
-- > do uid <- generateUID
-- >    store (makeSomethingWith uid)
-- >    return uid
--
-- Now we could use
--
-- > generateUID >>== (store . makeSomethingWith)
--
(>>==) :: (Monad m) => m a -> (a -> m b) -> m a
m >>== e = do
    v <- m
    _ <- e v
    return v

-- | This is just a ('>>==') with flipped arguments.
(==<<) :: (Monad m) => (a -> m b) -> m a -> m a
(==<<) = flip (>>==)
