-- | Examining a PostFam for a particular component of the main
-- posting (as opposed to the sibling postings) in the PostFam. For
-- some components, such as the payee, the posting might have one
-- piece of data while the TopLine has something else. These functions
-- will examine the Posting first and, if it has no information, use
-- the data from the TopLine if it is there.
module Penny.Lincoln.Queries where

import qualified Penny.Lincoln.Bits as B
import Penny.Lincoln.Family.Child (child, parent)
import qualified Penny.Lincoln.Transaction as T
import Penny.Lincoln.Balance (Balance, entryToBalance)
import qualified Data.Time as Time

-- | Uses the data from the Posting if it is set; otherwise, use the
-- data from the TopLine.
best ::
  (T.Posting pm -> Maybe a)
  -> (T.TopLine tm -> Maybe a)
  -> T.PostFam tm pm
  -> Maybe a
best fp ft c = case fp . child . T.unPostFam $ c of
  Just r -> Just r
  Nothing -> ft . parent . T.unPostFam $ c


payee :: T.PostFam tm pm -> Maybe B.Payee
payee = best T.pPayee T.tPayee

number :: T.PostFam tm pm -> Maybe B.Number
number = best T.pNumber T.tNumber

flag :: T.PostFam tm pm -> Maybe B.Flag
flag = best T.pFlag T.tFlag

postingMemo :: T.PostFam tm pm -> Maybe B.Memo
postingMemo = T.pMemo . child . T.unPostFam

transactionMemo :: T.PostFam tm pm -> Maybe B.Memo
transactionMemo = T.tMemo . parent . T.unPostFam

dateTime :: T.PostFam tm pm -> B.DateTime
dateTime = T.tDateTime . parent . T.unPostFam

localDay :: T.PostFam tm pm -> Time.Day
localDay = B.day . dateTime

account :: T.PostFam tm pm -> B.Account
account = T.pAccount . child . T.unPostFam

tags :: T.PostFam tm pm -> B.Tags
tags = T.pTags . child . T.unPostFam

entry :: T.PostFam tm pm -> B.Entry
entry = T.pEntry . child . T.unPostFam

balance :: T.PostFam tm pm -> Balance
balance = entryToBalance . entry

drCr :: T.PostFam tm pm -> B.DrCr
drCr = B.drCr . entry

amount :: T.PostFam tm pm -> B.Amount
amount = B.amount . entry

qty :: T.PostFam tm pm -> B.Qty
qty = B.qty . amount

commodity :: T.PostFam tm pm -> B.Commodity
commodity = B.commodity . amount

topMemoLine :: T.PostFam tm pm -> Maybe B.TopMemoLine
topMemoLine = T.tTopMemoLine . parent . T.unPostFam

topLineLine :: T.PostFam tm pm -> Maybe B.TopLineLine
topLineLine = T.tTopLineLine . parent . T.unPostFam

fileTransaction :: T.PostFam tm pm -> Maybe B.FileTransaction
fileTransaction = T.tFileTransaction . parent . T.unPostFam

postingLine :: T.PostFam tm pm -> Maybe B.PostingLine
postingLine = T.pPostingLine . child . T.unPostFam

side :: T.PostFam tm pm -> Maybe B.Side
side = B.side . amount

spaceBetween :: T.PostFam tm pm -> Maybe B.SpaceBetween
spaceBetween = B.spaceBetween . amount

meta :: T.PostFam tm pm -> pm
meta = T.pMeta . child . T.unPostFam
