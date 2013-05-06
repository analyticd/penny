-- | Examining a PostFam for a particular component of the main
-- posting (as opposed to the sibling postings) in the PostFam. For
-- some components, such as the payee, the posting might have one
-- piece of data while the TopLine has something else. These functions
-- will examine the Posting first and, if it has no information, use
-- the data from the TopLine if it is there.
module Penny.Lincoln.Queries where

import qualified Penny.Lincoln.Bits as B
import qualified Penny.Lincoln.Ents as E
import Penny.Lincoln.Balance (Balance, entryToBalance)
import qualified Data.Time as Time

-- | Uses the data from the Posting if it is set; otherwise, use the
-- data from the TopLine.
best
  :: (B.TopLineData -> Maybe a)
  -> (E.View B.PostingData -> Maybe a)
  -> E.ViewedEnt
  -> Maybe a
best fp ft vp = case fp . fst $ vp of
  Nothing -> ft . snd $ vp
  Just r -> Just r

payee :: E.ViewedEnt -> Maybe B.Payee
payee = best (B.tPayee . B.tlCore)
             (B.pPayee . B.pdCore . E.meta . E.headEnt)

number :: E.ViewedEnt -> Maybe B.Number
number = best (B.tNumber . B.tlCore)
              (B.pNumber . B.pdCore . E.meta . E.headEnt)

flag :: E.ViewedEnt -> Maybe B.Flag
flag = best (B.tFlag . B.tlCore)
            (B.pFlag . B.pdCore . E.meta . E.headEnt)

postingMemo :: E.ViewedEnt -> Maybe B.Memo
postingMemo = B.pMemo . B.pdCore . E.meta . E.headEnt . snd

transactionMemo :: E.ViewedEnt -> Maybe B.Memo
transactionMemo =  B.tMemo . B.tlCore . fst

dateTime :: E.ViewedEnt -> B.DateTime
dateTime = B.tDateTime . B.tlCore . fst

localDay :: E.ViewedEnt -> Time.Day
localDay = B.day . dateTime

account :: E.ViewedEnt -> B.Account
account = B.pAccount . B.pdCore . E.meta . E.headEnt . snd

tags :: E.ViewedEnt -> B.Tags
tags = B.pTags . B.pdCore . E.meta . E.headEnt . snd

entry :: E.ViewedEnt -> B.Entry
entry = E.entry . E.headEnt . snd

balance :: E.ViewedEnt -> Balance
balance = entryToBalance . entry

drCr :: E.ViewedEnt -> B.DrCr
drCr = B.drCr . entry

amount :: E.ViewedEnt -> B.Amount
amount = B.amount . entry

qty :: E.ViewedEnt -> B.Qty
qty = B.qty . amount

commodity :: E.ViewedEnt -> B.Commodity
commodity = B.commodity . amount

topMemoLine :: E.ViewedEnt -> Maybe B.TopMemoLine
topMemoLine = fmap B.tTopMemoLine . B.tlFileMeta . fst

topLineLine :: E.ViewedEnt -> Maybe B.TopLineLine
topLineLine = fmap B.tTopLineLine . B.tlFileMeta . fst

fileTransaction :: E.ViewedEnt -> Maybe B.FileTransaction
fileTransaction = fmap B.tFileTransaction . B.tlFileMeta . fst

postingLine :: E.ViewedEnt -> Maybe B.PostingLine
postingLine = fmap B.pPostingLine . B.pdFileMeta
              . E.meta . E.headEnt . snd

side :: E.ViewedEnt -> Maybe B.Side
side = B.pSide . B.pdCore . E.meta . E.headEnt . snd

spaceBetween :: E.ViewedEnt -> Maybe B.SpaceBetween
spaceBetween = B.pSpaceBetween . B.pdCore
               . E.meta . E.headEnt . snd

