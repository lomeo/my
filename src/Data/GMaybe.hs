module Data.GMaybe where

import Prelude hiding (maybe)
import qualified Prelude as P
import Control.Monad (MonadPlus(..))

class MaybeLike m where
    nothing     :: m a
    just        :: a -> m a
    maybe       :: b -> (a -> b) -> m a -> b

instance MaybeLike Maybe where
    nothing = Nothing
    just = Just
    maybe = P.maybe

isNothing, isJust :: MaybeLike m => m a -> Bool
isNothing = maybe True (const False)
isJust = not . isNothing

fromMaybe :: MaybeLike m => a -> m a -> a
fromMaybe n = maybe n id

-- listToMaybe :: MaybeLike m => [a] -> m a
-- listToMaybe 

maybeToMonad :: (MaybeLike m, MonadPlus f) => m a -> f a
maybeToMonad = maybe mzero return

catMaybes :: (MaybeLike m, MonadPlus f) => f (m a) -> f a
catMaybes = (>>= maybeToMonad)

mapMaybes :: (MaybeLike m, Functor f, MonadPlus f) => (a -> m b) -> f a -> f b
mapMaybes f = catMaybes . fmap f
