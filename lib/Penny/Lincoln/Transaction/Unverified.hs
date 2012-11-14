-- | Provides record types to hold the data that are in an unverified
-- transaction. Use these records along with the functions in
-- 'Penny.Lincoln.Transaction' to create Transactions. You can create
-- a Transaction only if the postings are balanced.
module Penny.Lincoln.Transaction.Unverified where

import qualified Penny.Lincoln.Bits as B
import qualified Penny.Lincoln.Meta as M

data TopLine = TopLine {
    tDateTime :: B.DateTime
  , tFlag     :: Maybe B.Flag
  , tNumber   :: Maybe B.Number
  , tPayee    :: Maybe B.Payee
  , tMemo     :: Maybe B.Memo
  , tMeta     :: M.TopLineMeta
  } deriving (Eq, Show)

data Posting = Posting {
    pPayee   :: Maybe B.Payee
  , pNumber  :: Maybe B.Number
  , pFlag    :: Maybe B.Flag
  , pAccount :: B.Account
  , pTags    :: B.Tags
  , pEntry   :: Maybe B.Entry
  , pMemo    :: Maybe B.Memo
  , pMeta    :: M.PostingMeta
  } deriving (Eq, Show)

entry :: Posting -> Maybe B.Entry
entry (Posting _ _ _ _ _ e _ _) = e
