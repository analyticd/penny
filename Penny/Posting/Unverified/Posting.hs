module Penny.Posting.Unverified.Posting where

import qualified Penny.Bits as B

data Cost =
  Blank
  | EntryOnly B.Entry
  | EntryPrice B.Entry B.Price

data Posting =
  Posting { payee :: B.Payee
          , number :: Maybe B.Number
          , cleared :: B.Cleared
          , account :: B.Account
          , cost :: Cost
          , tags :: B.Tags
          , memo :: Maybe B.Memo }

