module Penny.Posting.Unverified.Posting where

import qualified Penny.Bits as B
import qualified Penny.Bits.Entry as E

data Posting =
  Posting { payee :: Maybe B.Payee
          , number :: Maybe B.Number
          , flag :: Maybe B.Flag
          , account :: B.Account
          , entry :: Maybe E.Entry
          , tags :: B.Tags
          , memo :: Maybe B.Memo }
  deriving Show

