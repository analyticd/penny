-- | These are the bits that are "open"; that is, their constructors
-- are exported. This includes most bits. Some bits that have open
-- constructors are not in this module because they include other bits
-- that do not have exported constructors.

module Penny.Lincoln.Bits.Open where

import Data.List (sort)
import Data.Monoid ((<>))
import Data.Text (Text)
import qualified Data.Text as X
import qualified Penny.Lincoln.Equivalent as Ev
import Penny.Lincoln.Equivalent ((==~))
import qualified Penny.Lincoln.Serial as S

newtype SubAccount =
  SubAccount { unSubAccount :: Text }
  deriving (Eq, Ord, Show)

newtype Account = Account { unAccount :: [SubAccount] }
                  deriving (Eq, Show, Ord)

data Amount q = Amount
  { qty :: q
  , commodity :: Commodity }
  deriving (Eq, Show, Ord)

instance Functor Amount where
  fmap f (Amount q c) = Amount (f q) c

instance Ev.Equivalent q => Ev.Equivalent (Amount q) where
  equivalent (Amount q1 c1) (Amount q2 c2) =
    q1 ==~ q2 && c1 == c2
  compareEv (Amount q1 c1) (Amount q2 c2) =
    Ev.compareEv q1 q2 <> c1 `compare` c2

newtype Commodity =
  Commodity { unCommodity :: Text }
  deriving (Eq, Ord, Show)

data DrCr = Debit | Credit deriving (Eq, Show, Ord)


-- | Debit returns Credit; Credit returns Debit
opposite :: DrCr -> DrCr
opposite d = case d of
  Debit -> Credit
  Credit -> Debit

data Entry q = Entry
  { drCr :: DrCr
  , amount :: Amount q }
  deriving (Eq, Show, Ord)

instance Functor Entry where
  fmap f (Entry d a) = Entry d (fmap f a)

instance Ev.Equivalent q => Ev.Equivalent (Entry q) where
  equivalent (Entry d1 a1) (Entry d2 a2) =
    d1 == d2 && a1 ==~ a2
  compareEv (Entry d1 a1) (Entry d2 a2) =
    d1 `compare` d2 <> Ev.compareEv a1 a2

newtype Flag = Flag { unFlag :: Text }
             deriving (Eq, Show, Ord)

-- | There is one item in the list for each line of the memo. Do not
-- include newlines in the texts themselves. However there is nothing
-- to enforce this convention.
newtype Memo = Memo { unMemo :: [Text] }
             deriving (Eq, Show, Ord)

newtype Number = Number { unNumber :: Text }
                 deriving (Eq, Show, Ord)

newtype Payee = Payee { unPayee :: Text }
              deriving (Eq, Show, Ord)

newtype Tag = Tag { unTag :: Text }
                  deriving (Eq, Show, Ord)

newtype Tags = Tags { unTags :: [Tag] }
               deriving (Eq, Show, Ord)

-- | Tags are equivalent if they have the same tags (even if in a
-- different order).
instance Ev.Equivalent Tags where
  equivalent (Tags t1) (Tags t2) = sort t1 == sort t2
  compareEv (Tags t1) (Tags t2) =
    compare (sort t1) (sort t2)

-- Metadata

-- | The line number that the TopLine starts on (excluding the memo
-- accompanying the TopLine).
newtype TopLineLine = TopLineLine { unTopLineLine :: Int }
                      deriving (Eq, Show)

-- | The line number that the memo accompanying the TopLine starts on.
newtype TopMemoLine = TopMemoLine { unTopMemoLine :: Int }
                      deriving (Eq, Show)

-- | The commodity and and the quantity may appear with the commodity
-- on the left (e.g. USD 2.14) or with the commodity on the right
-- (e.g. 2.14 USD).
data Side
  = CommodityOnLeft
  | CommodityOnRight
  deriving (Eq, Show, Ord)

-- | There may or may not be a space in between the commodity and the
-- quantity.
data SpaceBetween
  = SpaceBetween
  | NoSpaceBetween
  deriving (Eq, Show, Ord)

-- | The name of the file in which a transaction appears.
newtype Filename = Filename { unFilename :: X.Text }
                   deriving (Eq, Show)

-- | The line number on which a price appears.
newtype PriceLine = PriceLine { unPriceLine :: Int }
                    deriving (Eq, Show)

-- | The line number on which a posting appears.
newtype PostingLine = PostingLine { unPostingLine :: Int }
                      deriving (Eq, Show)

-- | All postings are numbered in order, beginning with the first
-- posting in the first file and ending with the last posting
-- in the last file.
newtype GlobalPosting =
  GlobalPosting { unGlobalPosting :: S.Serial }
  deriving (Eq, Show)

-- | The postings in each file are numbered in order.
newtype FilePosting =
  FilePosting { unFilePosting :: S.Serial }
  deriving (Eq, Show)

-- | All transactions are numbered in order, beginning with the first
-- transaction in the first file and ending with the last transaction
-- in the last file.
newtype GlobalTransaction =
  GlobalTransaction { unGlobalTransaction :: S.Serial }
  deriving (Eq, Show)

-- | The transactions in each file are numbered in order.
newtype FileTransaction =
  FileTransaction { unFileTransaction :: S.Serial }
  deriving (Eq, Show)

