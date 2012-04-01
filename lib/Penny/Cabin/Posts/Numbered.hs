module Penny.Cabin.Posts.Numbered where

import qualified Penny.Cabin.Posts.Info as I
import qualified Penny.Liberty.Types as LT
import qualified Penny.Lincoln.Balance as Bal

data T = T {
  postingInfo :: LT.PostingInfo
  , balance :: Bal.Balance
  , postingNum :: I.PostingNum
  , revPostingNum :: I.RevPostingNum
  } deriving Show

toPostsInfo :: T -> I.VisibleNum -> I.T
toPostsInfo i vn = let pinfo = postingInfo i in I.T {
  I.postingBox = LT.postingBox pinfo
  , I.balance = balance i
  , I.postingNum = postingNum i
  , I.revPostingNum = revPostingNum i
  , I.fwdSeqUnsorted = LT.fwdSeqUnsorted pinfo
  , I.backSeqUnsorted = LT.backSeqUnsorted pinfo
  , I.fwdSeqSorted = LT.fwdSeqSorted pinfo
  , I.backSeqSorted = LT.backSeqSorted pinfo
  , I.visibleNum = vn }


  
