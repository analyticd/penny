module Penny.SeqDecs where

import qualified Penny.DecsGroup as DecsGroup
import Data.Sequence (Seq)

newtype T r = T { toSeq :: Seq (DecsGroup.T r) }
  deriving (Eq, Ord, Show)
