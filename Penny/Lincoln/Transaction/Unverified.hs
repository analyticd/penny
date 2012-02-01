module Penny.Lincoln.Transaction.Unverified where

import qualified Penny.Lincoln.Bits as B

data TopLine = TopLine B.DateTime B.Flag B.Number B.Payee B.Memo

data Posting =
  Posting B.Payee B.Number B.Flag B.Account
    B.Tags (Maybe B.Entry) B.Memo
