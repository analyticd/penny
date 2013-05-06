-- | Examining a PostFam for a particular component of the main
-- posting (as opposed to the sibling postings) in the PostFam. For
-- some components, such as the payee, the posting might have one
-- piece of data while the TopLine has something else. These functions
-- will examine the Posting first and, if it has no information, use
-- the data from the TopLine if it is there.
module Penny.Lincoln.Queries where

import qualified Penny.Lincoln.Bits as B
import qualified Penny.Lincoln.Transaction as T
import Penny.Lincoln.Balance (Balance, entryToBalance)
import qualified Data.Time as Time

-- | Uses the data from the Posting if it is set; otherwise, use the
-- data from the TopLine.
best
  :: (B.TopLineData -> Maybe a)
  -> (T.View B.PostingData -> Maybe a)
  -> T.ViewedPosting
  -> Maybe a
best fp ft vp = case fp . fst $ vp of
  Nothing -> ft . snd $ vp
  Just r -> Just r

payee :: T.ViewedPosting -> Maybe B.Payee
payee = best (B.tPayee . B.tlCore)
             (B.pPayee . B.pdCore . T.pMeta . T.headPosting)

number :: T.ViewedPosting -> Maybe B.Number
number = best (B.tNumber . B.tlCore)
              (B.pNumber . B.pdCore . T.pMeta . T.headPosting)

flag :: T.ViewedPosting -> Maybe B.Flag
flag = best (B.tFlag . B.tlCore)
            (B.pFlag . B.pdCore . T.pMeta . T.headPosting)

postingMemo :: T.ViewedPosting -> Maybe B.Memo
postingMemo = B.pMemo . B.pdCore . T.pMeta . T.headPosting . snd

transactionMemo :: T.ViewedPosting -> Maybe B.Memo
transactionMemo =  B.tMemo . B.tlCore . fst

dateTime :: T.ViewedPosting -> B.DateTime
dateTime = B.tDateTime . B.tlCore . fst

localDay :: T.ViewedPosting -> Time.Day
localDay = B.day . dateTime

account :: T.ViewedPosting -> B.Account
account = B.pAccount . B.pdCore . T.pMeta . T.headPosting . snd

tags :: T.ViewedPosting -> B.Tags
tags = B.pTags . B.pdCore . T.pMeta . T.headPosting . snd

entry :: T.ViewedPosting -> B.Entry
entry = T.pEntry . T.headPosting . snd

balance :: T.ViewedPosting -> Balance
balance = entryToBalance . entry

drCr :: T.ViewedPosting -> B.DrCr
drCr = B.drCr . entry

amount :: T.ViewedPosting -> B.Amount
amount = B.amount . entry

qty :: T.ViewedPosting -> B.Qty
qty = B.qty . amount

commodity :: T.ViewedPosting -> B.Commodity
commodity = B.commodity . amount

topMemoLine :: T.ViewedPosting -> Maybe B.TopMemoLine
topMemoLine = fmap B.tTopMemoLine . B.tlFileMeta . fst

topLineLine :: T.ViewedPosting -> Maybe B.TopLineLine
topLineLine = fmap B.tTopLineLine . B.tlFileMeta . fst

fileTransaction :: T.ViewedPosting -> Maybe B.FileTransaction
fileTransaction = fmap B.tFileTransaction . B.tlFileMeta . fst

postingLine :: T.ViewedPosting -> Maybe B.PostingLine
postingLine = fmap B.pPostingLine . B.pdFileMeta
              . T.pMeta . T.headPosting . snd

side :: T.ViewedPosting -> B.Side
side = B.pSide . B.pdCore . T.pMeta . T.headPosting . snd

spaceBetween :: T.ViewedPosting -> B.SpaceBetween
spaceBetween = B.pSpaceBetween . B.pdCore
               . T.pMeta . T.headPosting . snd

