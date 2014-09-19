module Penny.Lincoln.Memo where

import qualified Penny.Lincoln.Bar as B
import Data.Sequence

newtype T = T { toSeq :: Seq B.T }
  deriving (Eq, Ord, Show)
