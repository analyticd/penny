{-# LANGUAGE TemplateHaskell #-}
module Penny.Prefilt where


import Control.Lens hiding (pre)
import Penny.Clatch
import Penny.Ledger
import Penny.SeqUtil

data Prefilt l = Prefilt
  { _transaction :: TransactionL l
  , _posting :: View (Converted (PostingL l))
  }

makeLenses ''Prefilt
