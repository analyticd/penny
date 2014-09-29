module Penny.Harvest.Collect.PostingBox where

import qualified Penny.Harvest.Locate.Located as Located
import qualified Penny.Core.Serial.Global as Global
import qualified Penny.Core.Serial.Local as Local
import qualified Penny.Harvest.Collect.Memo.Posting as Memo
import qualified Penny.Tree.Posting as Tree.Posting
import qualified Penny.Core.Posting as Core.Posting
import qualified Penny.Tree.Posting.Error as Error

data T = T
  { posting :: Located.T Tree.Posting.T
  , local :: Local.T
  , global :: Global.T
  , memos :: Memo.T
  } deriving (Eq, Ord, Show)

toCore :: T -> Either Error.T Core.Posting.T
toCore t = do
  f <- Tree.Posting.toCore . Located.payload . posting $ t
  return $ f (Memo.toCore . memos $ t) (Located.location . posting $ t)
    (global t) (local t)
