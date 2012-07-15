module Penny.Lincoln.Queries where

import qualified Penny.Lincoln.Bits as B
import qualified Penny.Lincoln.Meta as M
import Penny.Lincoln.Family.Child (child, parent)
import qualified Penny.Lincoln.Transaction as T
import Penny.Lincoln.Balance (Balance, entryToBalance)
import qualified Penny.Lincoln.Family as F


type PostingChild = F.Child T.TopLine T.Posting

best ::
  (T.Posting -> Maybe a)
  -> (T.TopLine -> Maybe a)
  -> PostingChild
  -> Maybe a
best fp ft c = case fp . child $ c of
  Just r -> Just r
  Nothing -> ft . parent $ c


payee :: PostingChild -> Maybe B.Payee
payee = best T.pPayee T.tPayee

number :: PostingChild -> Maybe B.Number
number = best T.pNumber T.tNumber

flag :: PostingChild -> Maybe B.Flag
flag = best T.pFlag T.tFlag

postingMemo :: PostingChild -> B.Memo
postingMemo = T.pMemo . child

transactionMemo :: PostingChild -> B.Memo
transactionMemo = T.tMemo . parent

dateTime :: PostingChild -> B.DateTime
dateTime = T.tDateTime . parent

account :: PostingChild -> B.Account
account = T.pAccount . child

tags :: PostingChild -> B.Tags
tags = T.pTags . child

entry :: PostingChild -> B.Entry
entry = T.pEntry . child

balance :: PostingChild -> Balance
balance = entryToBalance . entry

drCr :: PostingChild -> B.DrCr
drCr = B.drCr . entry

amount :: PostingChild -> B.Amount
amount = B.amount . entry

qty :: PostingChild -> B.Qty
qty = B.qty . amount

commodity :: PostingChild -> B.Commodity
commodity = B.commodity . amount

postingMeta :: PostingChild -> M.PostingMeta
postingMeta = T.pMeta . child

topLineMeta :: PostingChild -> M.TopLineMeta
topLineMeta = T.tMeta . parent
