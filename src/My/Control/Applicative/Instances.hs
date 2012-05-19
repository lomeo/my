module My.Control.Applicative.Instances where

import Control.Applicative
import Control.Monad (ap)

import Data.Foldable (toList)
import Data.Sequence (Seq, (><))
import qualified Data.Sequence as S

instance Applicative Seq where
    pure = S.singleton
    (<*>) = ap

instance Alternative Seq where
    empty = S.empty
    (<|>) = (><)

newtype ZipSeq a = ZipSeq { getZipSeq :: Seq a }
    deriving (Functor)

instance Applicative ZipSeq where
    pure = ZipSeq . S.singleton
    ZipSeq s1 <*> ZipSeq s2 = ZipSeq $ S.fromList $ zipWith ($) (toList s1) (toList s2)
