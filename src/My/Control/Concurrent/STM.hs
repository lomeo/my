--------------------------------------------------------------------
-- |
-- Module    :  My.Control.Concurrent.STM
-- Copyright :  2009 (c) Dmitry Antonyuk
-- License   :  MIT
--
-- Maintainer:  Dmitry Antonyuk <lomeo.nuke@gmail.com>
-- Stability :  experimental
-- Portability: portable
--
-- Utilities for STM monad.
--
--------------------------------------------------------------------

module My.Control.Concurrent.STM (
    optionElse -- :: [STM a] -> STM a
) where

import Control.Monad.STM (STM, orElse)

-- | Compose a list of alternative STM actions.  If the first action
-- completes without retrying then it forms the result of the optionElse.
-- Otherwise, if the first action retries, then the second action is
-- tried in its place and so on. If all actions retry then the optionElse
-- as a whole retries.
optionElse :: [STM a] -> STM a
optionElse = foldr1 orElse

