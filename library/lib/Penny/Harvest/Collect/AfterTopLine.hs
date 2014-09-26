module Penny.Harvest.Collect.AfterTopLine where

import qualified Penny.Harvest.Collect.TransactionMemos
  as TransactionMemos
import qualified Penny.Tree.TopLine as TopLine
import qualified Penny.Core.Serial.Global as Global
import qualified Penny.Core.Serial.Local as Local
import qualified Penny.Harvest.Locate.Located as Located

-- | Just saw a TopLine, waiting for a Posting
data T = T
  { memos :: TransactionMemos.T
  , topLine :: Located.T TopLine.T
  , local :: Local.T
  , global :: Global.T
  } deriving (Eq, Ord, Show)
