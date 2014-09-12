module Penny.Tags where

import Data.Sequence
import qualified Penny.Tag as Tag

newtype T = T { toSeq :: Seq Tag.T }
  deriving (Eq, Ord, Show)
