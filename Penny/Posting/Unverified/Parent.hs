module Penny.Posting.Unverified.Parent where

import qualified Penny.Bits as B

data Parent =
  Parent { dateTime :: B.DateTime
         , flag :: Maybe B.Flag
         , number :: B.Number
         , payee :: B.Payee
         , memo :: B.Memo }
