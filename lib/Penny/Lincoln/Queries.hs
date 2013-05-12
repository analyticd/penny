-- | Examining a Posting for a particular component of the main
-- posting (as opposed to the sibling postings) in the Posting. For
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
  -> (E.Ents B.PostingData -> Maybe a)
  -> E.Posting
  -> Maybe a
best fp ft vp = case fp . fst . E.unPosting $ vp of
  Nothing -> ft . snd . E.unPosting $ vp
  Just r -> Just r

payee :: E.Posting -> Maybe B.Payee
payee = best (B.tPayee . B.tlCore)
             (B.pPayee . B.pdCore . E.meta . E.headEnt)

number :: E.Posting -> Maybe B.Number
number = best (B.tNumber . B.tlCore)
              (B.pNumber . B.pdCore . E.meta . E.headEnt)

flag :: E.Posting -> Maybe B.Flag
flag = best (B.tFlag . B.tlCore)
            (B.pFlag . B.pdCore . E.meta . E.headEnt)

postingMemo :: E.Posting -> Maybe B.Memo
postingMemo = B.pMemo . B.pdCore . E.meta . E.headEnt . snd . E.unPosting

transactionMemo :: E.Posting -> Maybe B.Memo
transactionMemo =  B.tMemo . B.tlCore . fst . E.unPosting

dateTime :: E.Posting -> B.DateTime
dateTime = B.tDateTime . B.tlCore . fst . E.unPosting

localDay :: E.Posting -> Time.Day
localDay = B.day . dateTime

account :: E.Posting -> B.Account
account = B.pAccount . B.pdCore . E.meta . E.headEnt . snd . E.unPosting

tags :: E.Posting -> B.Tags
tags = B.pTags . B.pdCore . E.meta . E.headEnt . snd . E.unPosting

entry :: E.Posting -> B.Entry
entry = E.entry . E.headEnt . snd . E.unPosting

balance :: E.Posting -> Balance
balance = entryToBalance . entry

drCr :: E.Posting -> B.DrCr
drCr = B.drCr . entry

amount :: E.Posting -> B.Amount
amount = B.amount . entry

qty :: E.Posting -> B.Qty
qty = B.qty . amount

commodity :: E.Posting -> B.Commodity
commodity = B.commodity . amount

topMemoLine :: E.Posting -> Maybe B.TopMemoLine
topMemoLine p = (B.tlFileMeta . fst . E.unPosting $ p) >>= B.tTopMemoLine

topLineLine :: E.Posting -> Maybe B.TopLineLine
topLineLine = fmap B.tTopLineLine . B.tlFileMeta . fst . E.unPosting

globalTransaction :: E.Posting -> Maybe B.GlobalTransaction
globalTransaction = B.tlGlobal . fst . E.unPosting

fileTransaction :: E.Posting -> Maybe B.FileTransaction
fileTransaction = fmap B.tFileTransaction . B.tlFileMeta . fst . E.unPosting

globalPosting :: E.Posting -> Maybe B.GlobalPosting
globalPosting = B.pdGlobal . E.meta . E.headEnt . snd . E.unPosting

filePosting :: E.Posting -> Maybe B.FilePosting
filePosting = fmap B.pFilePosting . B.pdFileMeta . E.meta
                   . E.headEnt . snd . E.unPosting

postingLine :: E.Posting -> Maybe B.PostingLine
postingLine = fmap B.pPostingLine . B.pdFileMeta
              . E.meta . E.headEnt . snd . E.unPosting

side :: E.Posting -> Maybe B.Side
side = B.pSide . B.pdCore . E.meta . E.headEnt . snd . E.unPosting

spaceBetween :: E.Posting -> Maybe B.SpaceBetween
spaceBetween = B.pSpaceBetween . B.pdCore
               . E.meta . E.headEnt . snd . E.unPosting

filename :: E.Posting -> Maybe B.Filename
filename = fmap B.tFilename . B.tlFileMeta . fst . E.unPosting
