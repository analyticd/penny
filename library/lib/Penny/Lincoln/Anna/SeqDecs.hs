module Penny.Lincoln.Anna.SeqDecs where

import qualified Penny.Lincoln.Anna.DecsGroup as DecsGroup
import Data.Sequence (Seq)

newtype T r = T { toSeq :: Seq (DecsGroup.T r) }
  deriving (Eq, Ord, Show)
