-- | Takes 'L.Line' from a group of files and adds serial numbers.

module Penny.Copper.Convert.Serializer where

import Data.Sequence
import qualified Penny.Copper.Tree.Line as L
import qualified Penny.Copper.Convert.TopLine as TL
import qualified Penny.Copper.Convert.Posting as P

newtype LineNum = LineNum { unLineNum :: Int }
  deriving (Eq, Ord, Show)
