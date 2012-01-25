module Penny.Posting.Parent where

import qualified Penny.Bits as B

data Parent =
  Parent { dateTime :: B.DateTime
         , cleared :: B.Cleared
         , number :: B.Number
         , payee :: B.Payee
         , memo :: B.Memo }
