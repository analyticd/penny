module Penny.Harvest.Collected.AfterTopLine where

import qualified Penny.Harvest.Collected.Memo.Transaction as Memos
import qualified Penny.Tree.TopLine as Tree.TopLine
import qualified Penny.Core.Serial.Global as Global
import qualified Penny.Core.Serial.Local as Local
import qualified Penny.Harvest.Locate.Located as Located
import qualified Penny.Tree.TopLine.Error as Error
import qualified Penny.Core.TopLine as Core.TopLine
import qualified Penny.Core.Clxn as Clxn

-- | Just saw a TopLine, waiting for a Posting
data T = T
  { memos :: Memos.T
  , topLine :: Located.T Tree.TopLine.T
  , local :: Local.T
  , global :: Global.T
  } deriving (Eq, Ord, Show)

toCore :: T -> Either Error.T (Clxn.T -> Core.TopLine.T)
toCore t = do
  f <- Tree.TopLine.toCore . Located.payload . topLine $ t
  return $ \clxn -> f (Memos.toCore . memos $ t)
    (Located.location . topLine $ t) clxn
    (global t) (local t)
