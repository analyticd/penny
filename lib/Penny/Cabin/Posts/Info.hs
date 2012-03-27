module Penny.Cabin.Posts.Info where

import qualified Penny.Liberty.Types as T
import qualified Penny.Lincoln.Boxes as B
import qualified Penny.Lincoln.Balance as Bal

-- | Every posting is assigned a number, beginning from zero,
-- corresponding to its ordinal number in the original list of
-- postings, *before* non-visible postings are removed.
newtype PostingNum = PostingNum { unPostingNum :: Int }
                     deriving (Show, Eq, Ord)

-- | Like PostingNum, but in reverse: numbering starts at the last
-- posting, at zero, and works its way backward.
newtype RevPostingNum =
  RevPostingNum { unRevPostingNum :: Int }
  deriving (Show, Eq, Ord)

-- | Each visible posting is assigned a number, starting from 0
-- up. Only visible postings receive this number.
newtype VisibleNum = VisibleNum { unVisibleNum :: Int }
                     deriving (Show, Eq, Ord)

data PostingInfo =
  PostingInfo { postingBox :: B.PostingBox
              , balance :: Bal.Balance
              , postingNum :: PostingNum
              , revPostingNum :: RevPostingNum
              , fwdSeqUnsorted :: T.FwdSeqUnsorted
              , backSeqUnsorted :: T.BackSeqUnsorted
              , fwdSeqSorted :: T.FwdSeqSorted
              , backSeqSorted :: T.BackSeqSorted }

fromLibertyInfo ::
  Bal.Balance
  -> PostingNum
  -> RevPostingNum
  -> T.PostingInfo
  -> PostingInfo
fromLibertyInfo b pn rpn tpi =
  PostingInfo { postingBox = T.postingBox tpi
              , balance = b
              , postingNum = pn
              , revPostingNum = rpn
              , fwdSeqUnsorted = T.fwdSeqUnsorted tpi
              , backSeqUnsorted = T.backSeqUnsorted tpi
              , fwdSeqSorted = T.fwdSeqSorted tpi
              , backSeqSorted = T.backSeqSorted tpi }

toLibertyInfo :: PostingInfo -> T.PostingInfo
toLibertyInfo p =
  T.PostingInfo { T.postingBox = postingBox p
                , T.fwdSeqUnsorted = fwdSeqUnsorted p
                , T.backSeqUnsorted = backSeqUnsorted p
                , T.fwdSeqSorted = fwdSeqSorted p
                , T.backSeqSorted = backSeqSorted p }

