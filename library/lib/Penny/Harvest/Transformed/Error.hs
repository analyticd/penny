module Penny.Harvest.Transformed.Error where

import qualified Penny.Tree.TopLine.Error as TopLine.Error
import qualified Penny.Harvest.Collected.AfterTopLine as AfterTopLine
import qualified Penny.Tree.Posting.Error as Posting.Error
import qualified Penny.Harvest.Collected.PostingBox as PostingBox
import qualified Penny.Core.Transaction.Error as Transaction.Error
import qualified Data.Sequence as S
import qualified Penny.Core.TopLine as TopLine
import qualified Penny.Core.Posting as Posting

data T
  = TopLine TopLine.Error.T AfterTopLine.T
  | Posting Posting.Error.T PostingBox.T
  | Transaction TopLine.T (S.Seq Posting.T) Transaction.Error.T
  deriving (Eq, Ord, Show)
