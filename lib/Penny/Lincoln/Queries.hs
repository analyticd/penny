module Penny.Lincoln.Queries where

import qualified Penny.Lincoln.Bits as B
import qualified Penny.Lincoln.Meta as M
import Penny.Lincoln.Family.Child (child, parent)
import qualified Penny.Lincoln.Transaction as T
import Penny.Lincoln.Balance (Balance, entryToBalance)

best ::
  (T.Posting -> Maybe a)
  -> (T.TopLine -> Maybe a)
  -> T.PostFam
  -> Maybe a
best fp ft c = case fp . child . T.unPostFam $ c of
  Just r -> Just r
  Nothing -> ft . parent . T.unPostFam $ c


payee :: T.PostFam -> Maybe B.Payee
payee = best T.pPayee T.tPayee

number :: T.PostFam -> Maybe B.Number
number = best T.pNumber T.tNumber

flag :: T.PostFam -> Maybe B.Flag
flag = best T.pFlag T.tFlag

postingMemo :: T.PostFam -> B.Memo
postingMemo = T.pMemo . child . T.unPostFam

transactionMemo :: T.PostFam -> B.Memo
transactionMemo = T.tMemo . parent . T.unPostFam

dateTime :: T.PostFam -> B.DateTime
dateTime = T.tDateTime . parent . T.unPostFam

account :: T.PostFam -> B.Account
account = T.pAccount . child . T.unPostFam

tags :: T.PostFam -> B.Tags
tags = T.pTags . child . T.unPostFam

entry :: T.PostFam -> B.Entry
entry = T.pEntry . child . T.unPostFam

balance :: T.PostFam -> Balance
balance = entryToBalance . entry

drCr :: T.PostFam -> B.DrCr
drCr = B.drCr . entry

amount :: T.PostFam -> B.Amount
amount = B.amount . entry

qty :: T.PostFam -> B.Qty
qty = B.qty . amount

commodity :: T.PostFam -> B.Commodity
commodity = B.commodity . amount

postingMeta :: T.PostFam -> M.PostingMeta
postingMeta = T.pMeta . child . T.unPostFam

topLineMeta :: T.PostFam -> M.TopLineMeta
topLineMeta = T.tMeta . parent . T.unPostFam
