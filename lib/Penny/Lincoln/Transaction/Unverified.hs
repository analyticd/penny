module Penny.Lincoln.Transaction.Unverified where

import qualified Penny.Lincoln.Bits as B

data TopLine m = TopLine
                 B.DateTime
                 (Maybe B.Flag)
                 (Maybe B.Number)
                 (Maybe B.Payee)
                 B.Memo
                 m
                 deriving (Eq, Show)

data Posting m = Posting
                 (Maybe B.Payee)
                 (Maybe B.Number) 
                 (Maybe B.Flag) 
                 B.Account
                 B.Tags
                 (Maybe B.Entry)
                 B.Memo
                 m
                 deriving (Eq, Show)

entry :: Posting m -> Maybe B.Entry
entry (Posting _ _ _ _ _ e _ _) = e
