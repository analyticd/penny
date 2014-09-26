module Penny.Harvest.Collect.Result where

import qualified Penny.Core.Clxn as Clxn
import Data.Sequence (Seq)
import Penny.Harvest.Locate.Located as Located
import qualified Penny.Harvest.Collect.Error as Error
import qualified Penny.Harvest.Collect.AfterTopLine as AfterTopLine
import qualified Penny.Harvest.Collect.AfterPosting as AfterPosting

data T = T
  { clxn :: Clxn.T
  , errors :: Seq (Located.T Error.T)
  , finalError :: Maybe Error.T
  , goods :: Seq (Either AfterTopLine.T AfterPosting.T)
  } deriving (Eq, Ord, Show)
