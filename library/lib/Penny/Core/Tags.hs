module Penny.Core.Tags where

import Data.Sequence
import qualified Penny.Core.Tag as Tag

newtype T = T { toSeq :: Seq Tag.T }
  deriving (Eq, Ord, Show)
