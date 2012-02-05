module Penny.Lincoln.Queries where

import qualified Penny.Lincoln.Bits as B
import Penny.Lincoln.Boxes ( PostingBox, postingBundle)
import Penny.Lincoln.Family.Child (child, parent)
import qualified Penny.Lincoln.Transaction as T
import Penny.Lincoln.Balance (Balance, entryToBalance)


posting :: PostingBox t p -> T.Posting
posting = child . postingBundle

topLine :: PostingBox t p -> T.TopLine
topLine = parent . postingBundle

best :: (T.Posting -> Maybe a)
        -> (T.TopLine -> Maybe a)
        -> PostingBox t p
        -> Maybe a
best fp ft b = case fp . posting $ b of
  (Just r) -> Just r
  Nothing -> ft . topLine $ b


payee :: PostingBox t p -> Maybe B.Payee
payee = best T.pPayee T.tPayee

number :: PostingBox t p -> Maybe B.Number
number = best T.pNumber T.tNumber

flag :: PostingBox t p -> Maybe B.Flag
flag = best T.pFlag T.tFlag

postingMemo :: PostingBox t p -> Maybe B.Memo
postingMemo = T.pMemo . posting

transactionMemo :: PostingBox t p -> Maybe B.Memo
transactionMemo = T.tMemo . topLine

dateTime :: PostingBox t p -> B.DateTime
dateTime = T.tDateTime . topLine

account :: PostingBox t p -> B.Account
account = T.pAccount . posting

tags :: PostingBox t p -> B.Tags
tags = T.pTags . posting

entry :: PostingBox t p -> B.Entry
entry = T.pEntry . posting

balance :: PostingBox t p -> Balance
balance = entryToBalance . entry

drCr :: PostingBox t p -> B.DrCr
drCr = B.drCr . entry

amount :: PostingBox t p -> B.Amount
amount = B.amount . entry

qty :: PostingBox t p -> B.Qty
qty = B.qty . amount

commodity :: PostingBox t p -> B.Commodity
commodity = B.commodity . amount
