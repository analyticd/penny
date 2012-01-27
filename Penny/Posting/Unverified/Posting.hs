module Penny.Posting.Unverified.Posting where

import qualified Penny.Bits as B
import qualified Penny.Bits.Entry as E
import qualified Penny.Bits.Price as P

data Posting =
  Posting { payee :: B.Payee
          , number :: Maybe B.Number
          , cleared :: B.Cleared
          , account :: B.Account
          , entry :: E.Entry
          , tags :: B.Tags
          , uid :: Maybe B.Uid
          , memo :: Maybe B.Memo }

