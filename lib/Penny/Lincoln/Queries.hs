module Penny.Lincoln.Queries where

import qualified Penny.Lincoln.Bits as B
import Penny.Lincoln.Family.Child (child, parent)
import qualified Penny.Lincoln.Transaction as T
import qualified Penny.Lincoln.Meta as M
import Penny.Lincoln.Balance (Balance, entryToBalance)
import qualified Penny.Lincoln.Family as F


type PostingChild tm pm = F.Child (T.TopLine tm) (T.Posting pm)

best ::
  (T.Posting pm -> Maybe a)
  -> (T.TopLine tm -> Maybe a)
  -> PostingChild tm pm
  -> Maybe a
best fp ft c = case fp . child $ c of
  Just r -> Just r
  Nothing -> ft . parent $ c


payee :: PostingChild tm pm -> Maybe B.Payee
payee = best T.pPayee T.tPayee

number :: PostingChild tm pm -> Maybe B.Number
number = best T.pNumber T.tNumber

flag :: PostingChild tm pm -> Maybe B.Flag
flag = best T.pFlag T.tFlag

postingMemo :: PostingChild tm pm -> B.Memo
postingMemo = T.pMemo . child

transactionMemo :: PostingChild tm pm -> B.Memo
transactionMemo = T.tMemo . parent

dateTime :: PostingChild tm pm -> B.DateTime
dateTime = T.tDateTime . parent

account :: PostingChild tm pm -> B.Account
account = T.pAccount . child

tags :: PostingChild tm pm -> B.Tags
tags = T.pTags . child

entry :: PostingChild tm pm -> B.Entry
entry = T.pEntry . child

balance :: PostingChild tm pm -> Balance
balance = entryToBalance . entry

drCr :: PostingChild tm pm -> B.DrCr
drCr = B.drCr . entry

amount :: PostingChild tm pm -> B.Amount
amount = B.amount . entry

qty :: PostingChild tm pm -> B.Qty
qty = B.qty . amount

commodity :: PostingChild tm pm -> B.Commodity
commodity = B.commodity . amount

postingMeta :: PostingChild tm pm -> pm
postingMeta = T.pMeta . child

topLineMeta :: PostingChild tm pm -> tm
topLineMeta = T.tMeta . parent
