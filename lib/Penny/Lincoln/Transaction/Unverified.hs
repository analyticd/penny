module Penny.Lincoln.Transaction.Unverified where

import qualified Penny.Lincoln.Bits as B
import qualified Penny.Lincoln.Meta as M

data TopLine = TopLine
               B.DateTime
               (Maybe B.Flag)
               (Maybe B.Number)
               (Maybe B.Payee)
               B.Memo
               M.TopLineMeta
             deriving (Eq, Show)

data Posting = Posting
                 (Maybe B.Payee)
                 (Maybe B.Number) 
                 (Maybe B.Flag) 
                 B.Account
                 B.Tags
                 (Maybe B.Entry)
                 B.Memo
                 M.PostingMeta
                 deriving (Eq, Show)

entry :: Posting -> Maybe B.Entry
entry (Posting _ _ _ _ _ e _ _) = e
