module Penny.Posting where

import Data.Text (Text)
import Penny.Common
import Penny.Serial

newtype SubAccount =
  SubAccount { unSubAccount :: Text }
  deriving (Eq, Ord, Show)

newtype Account = Account { unAccount :: [SubAccount] }
                  deriving (Eq, Show, Ord)

data PostingMeta = PostingMeta
  { pstgLine :: Line

  , globalSerial :: Serial
  -- ^ All postings are numbered in order, beginning with the first
  -- posting in the first file and ending with the last posting in the
  -- last file.

  , fileSerial :: Serial
  -- ^ The postings in each file are numbered in order.
  } deriving (Eq, Ord, Show)

newtype Tag = Tag { unTag :: Text }
                  deriving (Eq, Show, Ord)

newtype Tags = Tags { unTags :: [Tag] }
               deriving (Eq, Show, Ord)

data PostingData = PostingData
  { pstgMemo :: Memo
  , pstgNumber :: Number
  , pstgFlag :: Flag
  , pstgPayee :: Payee
  , pstgTags :: Tags
  , pstgAccount :: Account
  } deriving (Eq, Ord, Show)

data Posting = Posting
  { pstgData :: PostingData
  , pstgMeta :: Maybe PostingMeta
  } deriving (Eq, Ord, Show)

