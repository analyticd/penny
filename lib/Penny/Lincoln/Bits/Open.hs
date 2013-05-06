{-# LANGUAGE DeriveGeneric, CPP #-}

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

#ifdef test
import Test.QuickCheck (Arbitrary, Gen, arbitrary)
import qualified Test.QuickCheck as QC
import Control.Monad (liftM2)
#endif

#ifdef test
genText :: Gen Text
genText = fmap X.pack $ QC.oneof [ QC.listOf ascii, QC.listOf rest ]
  where
    ascii = QC.choose (toEnum 32, toEnum 126)
    rest = QC.choose (minBound, maxBound)
#endif

newtype SubAccount =
  SubAccount { unSubAccount :: Text }
  deriving (Eq, Ord, Show)

#ifdef test
instance Arbitrary SubAccount where
  arbitrary = fmap SubAccount genText
#endif

instance B.Binary SubAccount where
  put = B.put . XE.encodeUtf8 . unSubAccount
  get = fmap (SubAccount . XE.decodeUtf8) B.get

newtype Account = Account { unAccount :: [SubAccount] }
                  deriving (Eq, Show, Ord, Generic)

instance B.Binary Account

#ifdef test
instance Arbitrary Account where
  arbitrary = fmap Account arbitrary
#endif

data Amount = Amount { qty :: Q.Qty
                     , commodity :: Commodity }
              deriving (Eq, Show, Ord, Generic)

instance B.Binary Amount

#ifdef test
instance Arbitrary Amount where
  arbitrary = liftM2 Amount arbitrary arbitrary
#endif

newtype Commodity =
  Commodity { unCommodity :: Text }
  deriving (Eq, Ord, Show)

instance B.Binary Commodity where
  get = fmap (Commodity . XE.decodeUtf8) B.get
  put = B.put . XE.encodeUtf8 . unCommodity

#ifdef test
instance Arbitrary Commodity where
  arbitrary = fmap Commodity genText
#endif

data DrCr = Debit | Credit deriving (Eq, Show, Ord, Generic)

instance B.Binary DrCr

#ifdef test
instance Arbitrary DrCr where
  arbitrary = QC.elements [Debit, Credit]
#endif

-- | Debit returns Credit; Credit returns Debit
opposite :: DrCr -> DrCr
opposite d = case d of
  Debit -> Credit
  Credit -> Debit

data Entry = Entry { drCr :: DrCr
                   , amount :: Amount }
             deriving (Eq, Show, Ord, Generic)

instance B.Binary Entry

#ifdef test
instance Arbitrary Entry where
  arbitrary = liftM2 Entry arbitrary arbitrary
#endif

newtype Flag = Flag { unFlag :: Text }
             deriving (Eq, Show, Ord)

#ifdef test
instance Arbitrary Flag where
  arbitrary = fmap Flag genText
#endif

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

#ifdef test
instance Arbitrary Memo where
  arbitrary = fmap Memo $ QC.listOf genText
#endif

newtype Number = Number { unNumber :: Text }
                 deriving (Eq, Show, Ord)

#ifdef test
instance Arbitrary Number where
  arbitrary = fmap Number genText
#endif

instance B.Binary Number where
  get = fmap (Number . XE.decodeUtf8) B.get
  put = B.put . XE.encodeUtf8 . unNumber


newtype Payee = Payee { unPayee :: Text }
              deriving (Eq, Show, Ord)

#ifdef test
instance Arbitrary Payee where
  arbitrary = fmap Payee genText
#endif

instance B.Binary Payee where
  get = fmap (Payee . XE.decodeUtf8) B.get
  put = B.put . XE.encodeUtf8 . unPayee

newtype Tag = Tag { unTag :: Text }
                  deriving (Eq, Show, Ord)

#ifdef test
instance Arbitrary Tag where
  arbitrary = fmap Tag genText
#endif

instance B.Binary Tag where
  get = fmap (Tag . XE.decodeUtf8) B.get
  put = B.put . XE.encodeUtf8 . unTag

newtype Tags = Tags { unTags :: [Tag] }
               deriving (Eq, Show, Ord, Generic)

instance B.Binary Tags

#ifdef test
instance Arbitrary Tags where
  arbitrary = fmap Tags $ QC.listOf arbitrary
#endif

-- Metadata

-- | The line number that the TopLine starts on (excluding the memo
-- accompanying the TopLine).
newtype TopLineLine = TopLineLine { unTopLineLine :: Int }
                      deriving (Eq, Show, Generic)

instance B.Binary TopLineLine

#ifdef test
instance Arbitrary TopLineLine where
  arbitrary = fmap TopLineLine QC.arbitrarySizedBoundedIntegral
#endif

-- | The line number that the memo accompanying the TopLine starts on.
newtype TopMemoLine = TopMemoLine { unTopMemoLine :: Int }
                      deriving (Eq, Show, Generic)

instance B.Binary TopMemoLine

#ifdef test
instance Arbitrary TopMemoLine where
  arbitrary = fmap TopMemoLine QC.arbitrarySizedBoundedIntegral
#endif

-- | The commodity and and the quantity may appear with the commodity
-- on the left (e.g. USD 2.14) or with the commodity on the right
-- (e.g. 2.14 USD).
data Side
  = CommodityOnLeft
  | CommodityOnRight
  deriving (Eq, Show, Ord, Generic)

instance B.Binary Side

#ifdef test
instance Arbitrary Side where
  arbitrary = QC.elements [CommodityOnLeft, CommodityOnRight]
#endif

-- | There may or may not be a space in between the commodity and the
-- quantity.
data SpaceBetween
  = SpaceBetween
  | NoSpaceBetween
  deriving (Eq, Show, Ord, Generic)

instance B.Binary SpaceBetween

#ifdef test
instance Arbitrary SpaceBetween where
  arbitrary = QC.elements [SpaceBetween, NoSpaceBetween]
#endif

-- | The name of the file in which a transaction appears.
newtype Filename = Filename { unFilename :: X.Text }
                   deriving (Eq, Show)

instance B.Binary Filename where
  get = fmap (Filename . XE.decodeUtf8) B.get
  put = B.put . XE.encodeUtf8 . unFilename


#ifdef test
instance Arbitrary Filename where
  arbitrary = fmap Filename genText
#endif

-- | The line number on which a price appears.
newtype PriceLine = PriceLine { unPriceLine :: Int }
                    deriving (Eq, Show, Generic)

instance B.Binary PriceLine

#ifdef test
instance Arbitrary PriceLine where
  arbitrary = fmap PriceLine QC.arbitrarySizedBoundedIntegral
#endif

-- | The line number on which a posting appears.
newtype PostingLine = PostingLine { unPostingLine :: Int }
                      deriving (Eq, Show, Generic)

instance B.Binary PostingLine

#ifdef test
instance Arbitrary PostingLine where
  arbitrary = fmap PostingLine QC.arbitrarySizedBoundedIntegral
#endif

-- | All postings are numbered in order, beginning with the first
-- posting in the first file and ending with the last posting
-- in the last file.
newtype GlobalPosting =
  GlobalPosting { unGlobalPosting :: S.Serial }
  deriving (Eq, Show, Generic)

instance B.Binary GlobalPosting

#ifdef test
instance Arbitrary GlobalPosting where
  arbitrary = fmap GlobalPosting arbitrary
#endif

-- | The postings in each file are numbered in order.
newtype FilePosting =
  FilePosting { unFilePosting :: S.Serial }
  deriving (Eq, Show, Generic)

instance B.Binary FilePosting

#ifdef test
instance Arbitrary FilePosting where
  arbitrary = fmap FilePosting arbitrary
#endif

-- | All transactions are numbered in order, beginning with the first
-- transaction in the first file and ending with the last transaction
-- in the last file.
newtype GlobalTransaction =
  GlobalTransaction { unGlobalTransaction :: S.Serial }
  deriving (Eq, Show, Generic)

instance B.Binary GlobalTransaction

#ifdef test
instance Arbitrary GlobalTransaction where
  arbitrary = fmap GlobalTransaction arbitrary
#endif

-- | The transactions in each file are numbered in order.
newtype FileTransaction =
  FileTransaction { unFileTransaction :: S.Serial }
  deriving (Eq, Show, Generic)

#ifdef test
instance Arbitrary FileTransaction where
  arbitrary = fmap FileTransaction arbitrary
#endif

instance B.Binary FileTransaction

