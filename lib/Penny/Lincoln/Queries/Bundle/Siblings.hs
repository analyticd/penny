module Penny.Lincoln.Queries.Bundle.Siblings where

import Penny.Lincoln.Ents (Ent)
import qualified Penny.Lincoln.Ents as E
import Penny.Lincoln.Posting
import qualified Penny.Lincoln.Posting as P
import Penny.Lincoln.Transaction
import Penny.Lincoln.Pieces
import Penny.Lincoln.Common
import Penny.Lincoln.Serial

siblings :: Bundle -> [Ent Posting]
siblings = E.siblings . bunView

posting :: Bundle -> [Posting]
posting = fmap (map E.entMeta) siblings

postingData :: Bundle -> [PostingData]
postingData = fmap (map pstgData) posting

postingMeta :: Bundle -> [Maybe PostingMeta]
postingMeta = fmap (map pstgMeta) posting

memo :: Bundle -> [Memo]
memo = fmap (map pstgMemo) postingData

number :: Bundle -> [Number]
number = fmap (map pstgNumber) postingData

flag :: Bundle -> [Flag]
flag = fmap (map pstgFlag) postingData

payee :: Bundle -> [Payee]
payee = fmap (map pstgPayee) postingData

tags :: Bundle -> [Tags]
tags = fmap (map pstgTags) postingData

account :: Bundle -> [Account]
account = fmap (map pstgAccount) postingData

line :: Bundle -> [Maybe Line]
line = fmap (map (fmap pstgLine)) postingMeta

globalSerial :: Bundle -> [Maybe Serial]
globalSerial = fmap (map (fmap P.globalSerial)) postingMeta

fileSerial :: Bundle -> [Maybe Serial]
fileSerial = fmap (map (fmap P.fileSerial)) postingMeta
