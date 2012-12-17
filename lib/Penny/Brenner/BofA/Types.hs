module Penny.Brenner.BofA.Types where

import qualified Data.Map as M
import qualified Data.Time as Time
import qualified Penny.Copper.Render as R
import qualified Penny.Lincoln as L

-- | The date reported by the financial institution.
newtype Date = Date { unDate :: Time.Day }
  deriving (Eq, Show, Ord, Read)

-- | Reports changes in account balances. Avoids using /debit/ and
-- /credit/ as these terms are used differently by the bank than in
-- your ledger (that is, the bank reports it from their perspective,
-- not yours) so instead the terms /increase/ and /decrease/ are
-- used. IncDec is used to record the bank's transactions so
-- /increase/ and /decrease/ are used in the same way you would see
-- them on a bank statement, whether it's a credit card, loan,
-- checking account, etc.
data IncDec
  = Increase
  -- ^ Increases the account balance. For a checking or savings
  -- account, this is a deposit. For a credit card, this is a purchase.

  | Decrease
  -- ^ Decreases the account balance. On a credit card, this is a
  -- payment. On a checking account, this is a withdrawal.
  deriving (Eq, Show, Read)

-- | A unique number assigned by Brenner to identify each
-- posting. This is unique within a particular financial institution
-- account only.
newtype UNumber = UNumber { unUNumber :: Integer }
  deriving (Eq, Show, Ord, Read)

-- | For Brenner to work, the bank has to assign unique identifiers to
-- each transaction that it gives you for download. This is the
-- easiest reliable way to ensure duplicates are not processed
-- multiple times. (There are other ways to accomplish this, but they
-- are much harder and less reliable.) If the bank does not do this,
-- you can't use Brenner.
newtype FitId = FitId { unAmexId :: String }
  deriving (Eq, Show, Ord, Read)

-- | Some financial institutions assign a separate Payee in addition
-- to a description. Others just have a single Description field. If
-- this institution uses both, put something here. Brenner will prefer
-- the Payee if it is not zero length; then it will use the Desc.
newtype Payee = Payee { unPayee :: String }
  deriving (Eq, Show, Ord, Read)

-- | The transaction description. Some institutions assign only a
-- description (sometimes muddling a payee with long codes, some
-- dates, etc). Brenner prefers the Payee if there is one, and uses a
-- Desc otherwise.
newtype Desc =
  Desc { unDesc :: String }
  deriving (Eq, Show, Ord, Read)

-- | The amount of the transaction. Do not include any leading plus or
-- minus signs; this should be only digits and a decimal point.
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

-- | The Penny account holding postings for this financial
-- institution. For instance it might be @Assets:Checking@ if this is
-- your checking account, @Liabilities:Credit Card@, or whatever.
newtype PennyAcct = PennyAcct { unAmexAcct :: L.Account }
  deriving (Eq, Show)

-- | What the financial institution shows as an increase or decrease
-- has to be recorded as a debit or credit in the PennyAcct.
data Translator
  = IncreaseIsDebit
  -- ^ That is, when the financial institution shows a posting that
  -- increases your account balance, you record a debit. You will
  -- probably use this for deposit accounts, like checking and
  -- savings. These are asset accounts so if the balance goes up you
  -- record a debit in your ledger.

  | IncreaseIsCredit
  -- ^ That is, when the financial institution shows a posting that
  -- increases your account balance, you record a credit. You will
  -- probably use this for liabilities, such as credit cards and other
  -- loans.

  deriving (Eq, Show)

-- | The default account to place unclassified postings in. For
-- instance @Expenses:Unclassified@.
newtype DefaultAcct = DefaultAcct { unDefaultAcct :: L.Account }
  deriving (Eq, Show)

-- | The currency for all transactions, e.g. @$@.
newtype Currency = Currency { unCurrency :: L.Commodity }
  deriving (Eq, Show)

-- | A batch of settings representing a single financial institution
-- account.
data FitAcct = FitAcct
  { dbLocation :: DbLocation
  , pennyAcct :: PennyAcct
  , defaultAcct :: DefaultAcct
  , currency :: Currency
  , groupSpecs :: R.GroupSpecs
  , translator :: Translator
  } deriving Show

-- | Configuration for the Brenner program. You can optionally have
-- a default FitAcct, which is used if you do not specify any FitAcct on the
-- command line. You can also name any number of additional FitAccts. If
-- you do not specify a default FitAcct, you must specify a FitAcct on the
-- command line.

data Config = Config
  { defaultCard :: Maybe FitAcct
  , moreCards :: [(Name, FitAcct)]
  } deriving Show

newtype CSVLocation = CSVLocation { unCSVLocation :: String }
  deriving (Show, Eq)

newtype AllowNew = AllowNew { unAllowNew :: Bool }
  deriving (Show, Eq)
