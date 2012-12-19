module Types where

import qualified Data.Map as M
import qualified Data.Time as Time
import qualified Penny.Copper.Render as R
import qualified Penny.Lincoln as L

-- | The date Amex reports. From field 1.
newtype Date = Date { unDate :: Time.Day }
  deriving (Eq, Show, Ord, Read)

data IncDec
  = Increase
  -- ^ Increases the account balance (e.g. charges). Amex shows these
  -- without a sign.

  | Decrease
  -- ^ Decreases the account balance (e.g. payments). Amex shows these
  -- with a leading minus sign.
  deriving (Eq, Show, Read)

-- | A unique number assigned by Brenner.Amex to identify each Amex
-- posting. Use this rather than the AmexId because the Amex IDs are
-- very long.
newtype UNumber = UNumber { unUNumber :: Integer }
  deriving (Eq, Show, Ord, Read)

-- | Amex assigns a unique reference number to each posting (at least
-- I hope it's unique!) From field 14. Remove the double and single
-- quotes; otherwise, leave it as is.
newtype AmexId = AmexId { unAmexId :: String }
  deriving (Eq, Show, Ord, Read)

-- | From field 11. Remove the double quotes; otherwise, leave it
-- as is.
newtype Payee = Payee { unPayee :: String }
  deriving (Eq, Show, Ord, Read)

-- | From field 3. Look at the 'payee' field if that is what you
-- want; however, this field is also included because some
-- transactions such as payments do not have a payee. There will
-- be extra spaces in this field.
newtype Desc =
  Desc { unDesc :: String }
  deriving (Eq, Show, Ord, Read)

-- | From field 8. Do not include the leading minus sign if it is
-- there (that is reflected in IncDec.)
newtype Amount = Amount { unAmount :: String }
  deriving (Eq, Show, Ord, Read)

type DbMap = M.Map UNumber Posting
type DbList = [(UNumber, Posting)]

data Posting = Posting
  { date :: Date
  , desc :: Desc
  , incDec :: IncDec
  , amount :: Amount
  , payee :: Payee
  , amexId :: AmexId
  } deriving (Read, Show)

-- | Where is the database of postings?
newtype DbLocation = DbLocation { unDbLocation :: String }
  deriving (Eq, Show)

-- | A name used to refer to a batch of settings.
newtype Name = Name { unName :: String }
  deriving (Eq, Show)

-- | The Penny account holding Amex postings.
newtype AmexAcct = AmexAcct { unAmexAcct :: L.Account }
  deriving (Eq, Show)

-- | The default account to place unclassified Amex postings in.
newtype DefaultAcct = DefaultAcct { unDefaultAcct :: L.Account }
  deriving (Eq, Show)

-- | The currency for all Amex transactions.
newtype Currency = Currency { unCurrency :: L.Commodity }
  deriving (Eq, Show)

-- | A batch of settings representing a single Amex card account.
data Card = Card
  { dbLocation :: DbLocation
  , amexAcct :: AmexAcct
  , defaultAcct :: DefaultAcct
  , currency :: Currency
  , groupSpecs :: R.GroupSpecs
  } deriving Show

-- | Configuration for the penny-amex program. You can optionally have
-- a default Card, which is used if you do not specify any Card on the
-- command line. You can also name any number of additional Cards. If
-- you do not specify a default Card, you must specify a Card on the
-- command line.

data Config = Config
  { defaultCard :: Maybe Card
  , moreCards :: [(Name, Card)]
  } deriving Show

newtype CSVLocation = CSVLocation { unCSVLocation :: String }
  deriving (Show, Eq)

newtype AllowNew = AllowNew { unAllowNew :: Bool }
  deriving (Show, Eq)
