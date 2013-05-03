{-# LANGUAGE DeriveGeneric #-}

-- | These are the bits that are "open"; that is, their constructors
-- are exported. This includes most bits. Some bits that have open
-- constructors are not in this module because they include other bits
-- that do not have exported constructors.

module Penny.Lincoln.Bits.Open where

import Data.Text (Text)
import qualified Data.Text as X
import qualified Data.Text.Encoding as XE
import GHC.Generics (Generic)
import qualified Penny.Lincoln.Serial as S
import qualified Penny.Lincoln.Bits.Qty as Q
import qualified Data.Binary as B

newtype SubAccount =
  SubAccount { unSubAccount :: Text }
  deriving (Eq, Ord, Show)

instance B.Binary SubAccount where
  put = B.put . XE.encodeUtf8 . unSubAccount
  get = fmap (SubAccount . XE.decodeUtf8) B.get

newtype Account = Account { unAccount :: [SubAccount] }
                  deriving (Eq, Show, Ord, Generic)

instance B.Binary Account

data Amount = Amount { qty :: Q.Qty
                     , commodity :: Commodity
                     , side :: Maybe Side
                     , spaceBetween :: Maybe SpaceBetween }
              deriving (Eq, Show, Ord, Generic)

instance B.Binary Amount

newtype Commodity =
  Commodity { unCommodity :: Text }
  deriving (Eq, Ord, Show)

instance B.Binary Commodity where
  get = fmap (Commodity . XE.decodeUtf8) B.get
  put = B.put . XE.encodeUtf8 . unCommodity

data DrCr = Debit | Credit deriving (Eq, Show, Ord, Generic)

instance B.Binary DrCr

-- | Debit returns Credit; Credit returns Debit
opposite :: DrCr -> DrCr
opposite d = case d of
  Debit -> Credit
  Credit -> Debit

data Entry = Entry { drCr :: DrCr
                   , amount :: Amount }
             deriving (Eq, Show, Ord, Generic)

instance B.Binary Entry

newtype Flag = Flag { unFlag :: Text }
             deriving (Eq, Show, Ord)

instance B.Binary Flag where
  get = fmap (Flag . XE.decodeUtf8) B.get
  put = B.put . XE.encodeUtf8 . unFlag

-- | There is one item in the list for each line of the memo. Do not
-- include newlines in the texts themselves. However there is nothing
-- to enforce this convention.
newtype Memo = Memo { unMemo :: [Text] }
             deriving (Eq, Show, Ord)

instance B.Binary Memo where
  get = fmap (Memo . map XE.decodeUtf8) B.get
  put = B.put . map XE.encodeUtf8 . unMemo

newtype Number = Number { unNumber :: Text }
                 deriving (Eq, Show, Ord)

instance B.Binary Number where
  get = fmap (Number . XE.decodeUtf8) B.get
  put = B.put . XE.encodeUtf8 . unNumber

newtype Payee = Payee { unPayee :: Text }
              deriving (Eq, Show, Ord)

instance B.Binary Payee where
  get = fmap (Payee . XE.decodeUtf8) B.get
  put = B.put . XE.encodeUtf8 . unPayee

newtype Tag = Tag { unTag :: Text }
                  deriving (Eq, Show, Ord)

instance B.Binary Tag where
  get = fmap (Tag . XE.decodeUtf8) B.get
  put = B.put . XE.encodeUtf8 . unTag

newtype Tags = Tags { unTags :: [Tag] }
               deriving (Eq, Show, Ord, Generic)

instance B.Binary Tags

-- Metadata

-- | The line number that the TopLine starts on (excluding the memo
-- accompanying the TopLine).
newtype TopLineLine = TopLineLine { unTopLineLine :: Int }
                      deriving (Eq, Show, Generic)

instance B.Binary TopLineLine

-- | The line number that the memo accompanying the TopLine starts on.
newtype TopMemoLine = TopMemoLine { unTopMemoLine :: Int }
                      deriving (Eq, Show, Generic)

instance B.Binary TopMemoLine

-- | The commodity and and the quantity may appear with the commodity
-- on the left (e.g. USD 2.14) or with the commodity on the right
-- (e.g. 2.14 USD).
data Side
  = CommodityOnLeft
  | CommodityOnRight
  deriving (Eq, Show, Ord, Generic)

instance B.Binary Side

-- | There may or may not be a space in between the commodity and the
-- quantity.
data SpaceBetween
  = SpaceBetween
  | NoSpaceBetween
  deriving (Eq, Show, Ord, Generic)

instance B.Binary SpaceBetween

-- | The name of the file in which a transaction appears.
newtype Filename = Filename { unFilename :: X.Text }
                   deriving (Eq, Show)

instance B.Binary Filename where
  get = fmap (Filename . XE.decodeUtf8) B.get
  put = B.put . XE.encodeUtf8 . unFilename


-- | The line number on which a price appears.
newtype PriceLine = PriceLine { unPriceLine :: Int }
                    deriving (Eq, Show, Generic)

instance B.Binary PriceLine

-- | The line number on which a posting appears.
newtype PostingLine = PostingLine { unPostingLine :: Int }
                      deriving (Eq, Show, Generic)

instance B.Binary PostingLine

-- | All postings are numbered in order, beginning with the first
-- posting in the first file and ending with the last posting
-- in the last file.
newtype GlobalPosting =
  GlobalPosting { unGlobalPosting :: S.Serial }
  deriving (Eq, Show, Generic)

instance B.Binary GlobalPosting

-- | The postings in each file are numbered in order.
newtype FilePosting =
  FilePosting { unFilePosting :: S.Serial }
  deriving (Eq, Show, Generic)

instance B.Binary FilePosting

-- | All transactions are numbered in order, beginning with the first
-- transaction in the first file and ending with the last transaction
-- in the last file.
newtype GlobalTransaction =
  GlobalTransaction { unGlobalTransaction :: S.Serial }
  deriving (Eq, Show, Generic)

instance B.Binary GlobalTransaction

-- | The transactions in each file are numbered in order.
newtype FileTransaction =
  FileTransaction { unFileTransaction :: S.Serial }
  deriving (Eq, Show, Generic)

instance B.Binary FileTransaction
