module Penny.Lincoln.Tags where

import Data.Sequence
import qualified Penny.Lincoln.Tag as Tag

newtype T = T { toSeq :: Seq Tag.T }
  deriving (Eq, Ord, Show)
