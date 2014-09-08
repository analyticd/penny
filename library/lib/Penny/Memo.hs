module Penny.Memo where

import qualified Penny.Bar as B
import Data.Sequence

newtype T = T { toSeq :: Seq B.T }
  deriving (Eq, Ord, Show)
