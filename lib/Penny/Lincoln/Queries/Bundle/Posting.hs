module Penny.Lincoln.Queries.Bundle.Posting where

import qualified Penny.Lincoln.Posting as P
import Penny.Lincoln.Serial
import Penny.Lincoln.Ents
import Penny.Lincoln.Trio
import Penny.Lincoln.Transaction
import Penny.Lincoln.Decimal
import Penny.Lincoln.Common
import Penny.Lincoln.Pieces

ent :: Bundle -> Ent P.Posting
ent = viewCurrent . bunView

posting :: Bundle -> P.Posting
posting = entMeta . ent

postingData :: Bundle -> P.PostingData
postingData = P.pstgData . posting

postingMeta :: Bundle -> Maybe P.PostingMeta
postingMeta = P.pstgMeta . posting

trio :: Bundle -> Trio
trio = entToTrio . ent

memo :: Bundle -> Memo
memo = P.pstgMemo . postingData

number :: Bundle -> Number
number = P.pstgNumber . postingData

flag :: Bundle -> Flag
flag = P.pstgFlag . postingData

payee :: Bundle -> Payee
payee = P.pstgPayee . postingData

tags :: Bundle -> P.Tags
tags = P.pstgTags . postingData

qty :: Bundle -> Qty
qty = entConcrete . ent

commodity :: Bundle -> Commodity
commodity = entCommodity . ent

entrio :: Bundle -> Entrio
entrio = entTrio . ent

account :: Bundle -> P.Account
account = P.pstgAccount . postingData

line :: Bundle -> Maybe Line
line = fmap P.pstgLine . postingMeta

globalSerial :: Bundle -> Maybe Serial
globalSerial = fmap P.globalSerial . postingMeta

fileSerial :: Bundle -> Maybe Serial
fileSerial = fmap P.fileSerial . postingMeta
