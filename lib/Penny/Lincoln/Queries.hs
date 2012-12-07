module Penny.Lincoln.Queries where

import qualified Penny.Lincoln.Bits as B
import Penny.Lincoln.Family.Child (child, parent)
import qualified Penny.Lincoln.Transaction as T
import Penny.Lincoln.Balance (Balance, entryToBalance)
import qualified Data.Time as Time

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

postingMemo :: T.PostFam -> Maybe B.Memo
postingMemo = T.pMemo . child . T.unPostFam

transactionMemo :: T.PostFam -> Maybe B.Memo
transactionMemo = T.tMemo . parent . T.unPostFam

dateTime :: T.PostFam -> B.DateTime
dateTime = T.tDateTime . parent . T.unPostFam

localDay :: T.PostFam -> Time.Day
localDay = B.day . dateTime

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

topMemoLine :: T.PostFam -> Maybe B.TopMemoLine
topMemoLine = T.tTopMemoLine . parent . T.unPostFam

topLineLine :: T.PostFam -> Maybe B.TopLineLine
topLineLine = T.tTopLineLine . parent . T.unPostFam

filename :: T.PostFam -> Maybe B.Filename
filename = T.tFilename . parent . T.unPostFam

globalTransaction :: T.PostFam -> Maybe B.GlobalTransaction
globalTransaction = T.tGlobalTransaction . parent . T.unPostFam

fileTransaction :: T.PostFam -> Maybe B.FileTransaction
fileTransaction = T.tFileTransaction . parent . T.unPostFam

postingLine :: T.PostFam -> Maybe B.PostingLine
postingLine = T.pPostingLine . child . T.unPostFam

side :: T.PostFam -> Maybe B.Side
side = T.pSide . child . T.unPostFam

spaceBetween :: T.PostFam -> Maybe B.SpaceBetween
spaceBetween = T.pSpaceBetween . child . T.unPostFam

globalPosting :: T.PostFam -> Maybe B.GlobalPosting
globalPosting = T.pGlobalPosting . child . T.unPostFam

filePosting :: T.PostFam -> Maybe B.FilePosting
filePosting = T.pFilePosting . child . T.unPostFam
