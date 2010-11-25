--------------------------------------------------------------------
-- |
-- Module    :  My.Control.Concurrent.MaybeChan
-- Copyright :  2009 (c) Dmitry Antonyuk
-- License   :  MIT
--
-- Maintainer:  Dmitry Antonyuk <lomeo.nuke@gmail.com>
-- Stability :  experimental
-- Portability: portable
--
-- 'MaybeChan' is a @Chan@ that works over 'Maybe' values.
--
--------------------------------------------------------------------

module My.Control.Concurrent.MaybeChan (
    -- * Wrappers
      MaybeChan
    , newChan
    , readChan
    , writeChan
    , writeList2Chan
    -- * Extensions
    , closeChan
    , whileReadChan
) where

import qualified Control.Concurrent as CC

import My.Control.Monad (whenJustM)

newtype MaybeChan a = MC { toChan :: CC.Chan (Maybe a) }

whileReadChan :: MaybeChan a -> (a -> IO ()) -> IO ()
whileReadChan (MC chan) act = loop
    where
        loop = whenJustM (CC.readChan chan) (\v -> act v >> loop)

newChan :: IO (MaybeChan a)
newChan = MC `fmap` CC.newChan

readChan :: MaybeChan a -> IO (Maybe a)
readChan = CC.readChan . toChan

writeChan :: MaybeChan a -> a -> IO ()
writeChan (MC chan) = CC.writeChan chan . Just

closeChan :: MaybeChan a -> IO ()
closeChan (MC chan) = CC.writeChan chan Nothing

writeList2Chan :: MaybeChan a -> [a] -> IO ()
writeList2Chan (MC chan) = CC.writeList2Chan chan . map Just
