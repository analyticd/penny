module Penny.Harvest.Collect.Result where

import qualified Penny.Core.Clxn as Clxn
import Data.Sequence (Seq)
import qualified Penny.Harvest.Collect.Error as Error
import qualified Penny.Harvest.Collect.AfterTopLine as AfterTopLine
import qualified Penny.Harvest.Collect.AfterPosting as AfterPosting

data T = T
  { clxn :: Clxn.T
  , error :: Error.T
  , goods :: Seq (Either AfterTopLine.T AfterPosting.T)
  } deriving (Eq, Ord, Show)
