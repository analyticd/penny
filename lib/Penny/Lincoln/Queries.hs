module Penny.Lincoln.Queries where

import qualified Penny.Lincoln.Bits as B
import Penny.Lincoln.Boxes ( PostingBox, postingBundle, metaBundle)
import Penny.Lincoln.Family.Child (child, parent)
import qualified Penny.Lincoln.Transaction as T
import qualified Penny.Lincoln.Meta as M
import Penny.Lincoln.Balance (Balance, entryToBalance)


posting :: PostingBox -> T.Posting
posting = child . postingBundle

topLine :: PostingBox -> T.TopLine
topLine = parent . postingBundle

best :: (T.Posting -> Maybe a)
        -> (T.TopLine -> Maybe a)
        -> PostingBox
        -> Maybe a
best fp ft b = case fp . posting $ b of
  (Just r) -> Just r
  Nothing -> ft . topLine $ b


payee :: PostingBox -> Maybe B.Payee
payee = best T.pPayee T.tPayee

number :: PostingBox -> Maybe B.Number
number = best T.pNumber T.tNumber

flag :: PostingBox -> Maybe B.Flag
flag = best T.pFlag T.tFlag

postingMemo :: PostingBox -> Maybe B.Memo
postingMemo = T.pMemo . posting

transactionMemo :: PostingBox -> Maybe B.Memo
transactionMemo = T.tMemo . topLine

dateTime :: PostingBox -> B.DateTime
dateTime = T.tDateTime . topLine

account :: PostingBox -> B.Account
account = T.pAccount . posting

tags :: PostingBox -> B.Tags
tags = T.pTags . posting

entry :: PostingBox -> B.Entry
entry = T.pEntry . posting

balance :: PostingBox -> Balance
balance = entryToBalance . entry

drCr :: PostingBox -> B.DrCr
drCr = B.drCr . entry

amount :: PostingBox -> B.Amount
amount = B.amount . entry

qty :: PostingBox -> B.Qty
qty = B.qty . amount

commodity :: PostingBox -> B.Commodity
commodity = B.commodity . amount

postingMeta :: PostingBox -> Maybe M.PostingMeta
postingMeta pb = metaBundle pb >>= return . child

postingLine :: PostingBox -> Maybe M.PostingLine
postingLine pb = postingMeta pb >>= M.postingLine

postingFormat :: PostingBox -> Maybe M.Format
postingFormat pb = postingMeta pb >>= M.postingFormat

topLineMeta :: PostingBox -> Maybe M.TopLineMeta
topLineMeta pb = metaBundle pb >>= return . parent

topMemoLine :: PostingBox -> Maybe M.TopMemoLine
topMemoLine pb = topLineMeta pb >>= M.topMemoLine

topLineLine :: PostingBox -> Maybe M.TopLineLine
topLineLine pb = topLineMeta pb >>= M.topLineLine

filename :: PostingBox -> Maybe M.Filename
filename pb = topLineMeta pb >>= M.filename
