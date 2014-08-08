module Penny.Posting where

import Penny.Trio
import Data.Text (Text)
import Penny.Common
import Penny.Serial

newtype SubAccount =
  SubAccount { unSubAccount :: Text }
  deriving (Eq, Ord, Show)

newtype Account = Account { unAccount :: [SubAccount] }
                  deriving (Eq, Show, Ord)

newtype Tag = Tag { unTag :: Text }
                  deriving (Eq, Show, Ord)

newtype Tags = Tags { unTags :: [Tag] }
               deriving (Eq, Show, Ord)

data Posting = Posting
  { pstgMemo :: Memo
  , pstgNumber :: Number
  , pstgFlag :: Flag
  , pstgPayee :: Payee
  , pstgTags :: Tags
  , pstgAccount :: Account
  , pstgLocation :: Location
  , pstgGlobalSerial :: Serial
  -- ^ All postings are numbered in order, beginning with the first
  -- posting in the first file and ending with the last posting in the
  -- last file.

  , pstgClxnSerial :: Serial
  -- ^ The postings in each collection are numbered in order.

  , pstgTrio :: Trio

  } deriving (Eq, Ord, Show)

