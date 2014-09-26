module Penny.Harvest.Collect.PostingBox where

import qualified Penny.Harvest.Locate.Located as Located
import qualified Penny.Core.Serial.Global as Global
import qualified Penny.Core.Serial.Local as Local
import Data.Sequence (Seq)
import qualified Penny.Tree.Memo.Posting as Memo
import qualified Penny.Tree.Posting as Posting

data T = T
  { posting :: Located.T Posting.T
  , local :: Local.T
  , global :: Global.T
  , memos :: Seq (Located.T Memo.T)
  } deriving (Eq, Ord, Show)
