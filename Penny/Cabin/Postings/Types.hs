-- | Data types used throughout the Postings report.
module Penny.Cabin.Postings.Types where

import qualified Data.Array as A
import qualified Penny.Liberty.Types as T
import qualified Penny.Lincoln.Boxes as B
import qualified Penny.Lincoln.Balance as Bal

newtype PostingNum = PostingNum { unPostingNum :: Int }
                     deriving (Show, Eq, Ord)

newtype RevPostingNum =
  RevPostingNum { unRevPostingNum :: Int }
  deriving (Show, Eq, Ord)

newtype VisibleNum = VisibleNum { unVisibleNum :: Int }
                     deriving (Show, Eq, Ord, A.Ix)

newtype ClaimedWidth = ClaimedWidth { unClaimedWidth :: Int }
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

