module Penny.Core.Memo where

import qualified Penny.Core.Bar as B
import Data.Sequence

newtype T = T { toSeq :: Seq B.T }
  deriving (Eq, Ord, Show)
