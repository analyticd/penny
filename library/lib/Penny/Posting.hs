module Penny.Posting where

import Penny.Trio
import Data.Text (Text)
import Penny.Common
import Penny.Serial
import Data.Sequence (Seq)

newtype SubAccount =
  SubAccount { unSubAccount :: Text }
  deriving (Eq, Ord, Show)

newtype Account = Account { unAccount :: Seq SubAccount }
                  deriving (Eq, Show, Ord)

newtype Tag = Tag { unTag :: Text }
                  deriving (Eq, Show, Ord)

newtype Tags = Tags { unTags :: Seq Tag }
               deriving (Eq, Show, Ord)

data Posting = Posting
  { pstgMemo :: Memo
  , pstgNumber :: Maybe Number
  , pstgFlag :: Maybe Flag
  , pstgPayee :: Maybe Payee
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

