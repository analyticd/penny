-- | Data types used throughout the Postings report.
module Penny.Cabin.Postings.Types where

import qualified Data.Array as A
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
              , revPostingNum :: RevPostingNum }
