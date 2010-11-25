-----------------------------------------------------------------------------
-- |
-- Module      :  My.Control.Concurrent
-- Copyright   :  (c) Dmitry Antonyuk 2009
-- License     :  MIT
--
-- Maintainer  :  lomeo.nuke@gmail.com
-- Stability   :  experimental
-- Portability :  portable
--
-- Utilities for multithreading programming.
--
-----------------------------------------------------------------------------
module My.Control.Concurrent (
    spawn -- :: IO () -> IO ThreadId
) where

import Control.Concurrent (ThreadId, forkIO, myThreadId)
import Control.Exception (throwTo)

-- | 'spawn' runs an IO action in a separate thread.
-- If an action throws an exception it will be rethrown to parent thread.
spawn :: IO () -> IO ThreadId
spawn act = do
    tId <- myThreadId
    forkIO $ act `catch` throwTo tId

