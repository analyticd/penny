module Penny.Lincoln.Transaction.Unverified where

import qualified Penny.Lincoln.Bits as B

data TopLine = TopLine
                 B.DateTime
                 (Maybe B.Flag)
                 (Maybe B.Number)
                 (Maybe B.Payee)
                 (Maybe B.Memo)
                 deriving Show

data Posting = Posting
                 (Maybe B.Payee)
                 (Maybe B.Number) 
                 (Maybe B.Flag) 
                 B.Account
                 B.Tags
                 (Maybe B.Entry)
                 (Maybe B.Memo)
                 deriving Show

entry :: Posting -> Maybe B.Entry
entry (Posting _ _ _ _ _ e _) = e
