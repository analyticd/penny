module Penny.Posting.Parent where

import qualified Penny.Bits as B

data Parent =
  Parent { dateTime :: B.DateTime
         , flag :: Maybe B.Flag
         , number :: Maybe B.Number
         , payee :: Maybe B.Payee
         , memo :: Maybe B.Memo }
  deriving Show
