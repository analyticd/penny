module Penny.Posting.TopLine where

import qualified Penny.Bits as B
import qualified Penny.Bits.DateTime as DT

data TopLine =
  TopLine { dateTime :: DT.DateTime
          , flag :: Maybe B.Flag
          , number :: Maybe B.Number
          , payee :: Maybe B.Payee
          , memo :: Maybe B.Memo }
  deriving Show
