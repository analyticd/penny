module Penny.Posting.Parent where

import qualified Penny.Bits as B
import qualified Penny.Bits.DateTime as DT

data Parent =
  Parent { dateTime :: DT.DateTime
         , flag :: Maybe B.Flag
         , number :: Maybe B.Number
         , payee :: Maybe B.Payee
         , memo :: Maybe B.Memo }
  deriving Show
