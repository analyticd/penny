module Penny.Brenner.Types
  ( Date(..)
  , IncDec(..)
  , UNumber(..)
  , FitId(..)
  , Payee(..)
  , Desc(..)
  , Amount(unAmount)
  , mkAmount
  , translate
  , DbMap
  , DbList
  , Posting(..)
  , DbLocation(..)
  , Name(..)
  , PennyAcct(..)
  , Translator(..)
  , DefaultAcct(..)
  , Currency(..)
  , FitAcct(..)
  , Config(..)
  , FitFileLocation(..)
  , AllowNew(..)
  ) where

import Control.Applicative ((<$>), (<*>))
import qualified Control.Monad.Exception.Synchronous as Ex
import qualified Data.Map as M
import qualified Data.Time as Time
import qualified Penny.Copper.Render as R
import qualified Penny.Lincoln as L
import Data.Text (Text, pack, unpack)
import qualified Data.Text.Encoding as E
import qualified Data.Serialize as S

-- | The date reported by the financial institution.
newtype Date = Date { unDate :: Time.Day }
  deriving (Eq, Show, Ord, Read)

instance S.Serialize Date where
  put = S.put . show . unDate
  get = Date <$> (read <$> S.get)

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

instance S.Serialize IncDec where
  put x = case x of
    Increase -> S.putWord8 0
    Decrease -> S.putWord8 1
  get = S.getWord8 >>= f
    where
      f x = case x of
        0 -> return Increase
        1 -> return Decrease
        _ -> fail "read IncDec error"

-- | A unique number assigned by Brenner to identify each
-- posting. This is unique within a particular financial institution
-- account only.
newtype UNumber = UNumber { unUNumber :: Integer }
  deriving (Eq, Show, Ord, Read)

instance S.Serialize UNumber where
  put = S.put . unUNumber
  get = UNumber <$> S.get

putText :: Text -> S.Put
putText = S.put . E.encodeUtf8

getText :: S.Get Text
getText = S.get >>= f
  where
    f bs = case E.decodeUtf8' bs of
      Left _ -> fail "text reading failed"
      Right x -> return x


-- | For Brenner to work, the bank has to assign unique identifiers to
-- each transaction that it gives you for download. This is the
-- easiest reliable way to ensure duplicates are not processed
-- multiple times. (There are other ways to accomplish this, but they
-- are much harder and less reliable.) If the bank does not do this,
-- you can't use Brenner.
newtype FitId = FitId { unFitId :: Text }
  deriving (Eq, Show, Ord, Read)

instance S.Serialize FitId where
  put = putText . unFitId
  get = FitId <$> getText

-- | Some financial institutions assign a separate Payee in addition
-- to a description. Others just have a single Description field. If
-- this institution uses both, put something here. Brenner will prefer
-- the Payee if it is not zero length; then it will use the Desc.
newtype Payee = Payee { unPayee :: Text }
  deriving (Eq, Show, Ord, Read)

instance S.Serialize Payee where
  put = putText . unPayee
  get = Payee <$> getText

-- | The transaction description. Some institutions assign only a
-- description (sometimes muddling a payee with long codes, some
-- dates, etc). Brenner prefers the Payee if there is one, and uses a
-- Desc otherwise.
newtype Desc =
  Desc { unDesc :: Text }
  deriving (Eq, Show, Ord, Read)

instance S.Serialize Desc where
  put = putText . unDesc
  get = Desc <$> getText

-- | The amount of the transaction. Do not include any leading plus or
-- minus signs; this should be only digits and a decimal point.
newtype Amount = Amount { unAmount :: Text }
  deriving (Eq, Show, Ord, Read)

instance S.Serialize Amount where
  put = putText . unAmount
  get = getText >>= f
    where
      f x = case mkAmount . unpack $ x of
        Nothing -> fail $ "failed to load amount: " ++ unpack x
        Just a -> return a

-- | Ensures that incoming Amounts have only digits and (up to) one
-- decimal point.
mkAmount :: String -> Maybe Amount
mkAmount s =
  let isDigit c = c >= '0' && c <= '9'
      (_, rs) = span isDigit s
  in case rs of
      "" -> if not . null $ s
            then return . Amount . pack $ s
            else Nothing
      '.':rest -> if all isDigit rest
                  then return . Amount . pack $ s
                  else Nothing
      _ -> Nothing

translate
  :: IncDec
  -> Translator
  -> L.DrCr
translate Increase IncreaseIsDebit = L.Debit
translate Increase IncreaseIsCredit = L.Credit
translate Decrease IncreaseIsDebit = L.Credit
translate Decrease IncreaseIsCredit = L.Debit

type DbMap = M.Map UNumber Posting
type DbList = [(UNumber, Posting)]

data Posting = Posting
  { date :: Date
  , desc :: Desc
  , incDec :: IncDec
  , amount :: Amount
  , payee :: Payee
  , fitId :: FitId
  } deriving (Read, Show)


instance S.Serialize Posting where
  put x = S.put (date x)
          >> S.put (desc x)
          >> S.put (incDec x)
          >> S.put (amount x)
          >> S.put (payee x)
          >> S.put (fitId x)
  get = Posting
        <$> S.get
        <*> S.get
        <*> S.get
        <*> S.get
        <*> S.get
        <*> S.get

-- | Where is the database of postings?
newtype DbLocation = DbLocation { unDbLocation :: Text }
  deriving (Eq, Show)

-- | A name used to refer to a batch of settings.
newtype Name = Name { unName :: Text }
  deriving (Eq, Show)

-- | The Penny account holding postings for this financial
-- institution. For instance it might be @Assets:Checking@ if this is
-- your checking account, @Liabilities:Credit Card@, or whatever.
newtype PennyAcct = PennyAcct { unPennyAcct :: L.Account }
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

  , side :: L.Side
  -- ^ When creating new transactions, the commodity will be on this
  -- side

  , spaceBetween :: L.SpaceBetween
  -- ^ When creating new transactions, is there a space between the
  -- commodity and the quantity

  , parser :: ( String
              , FitFileLocation -> IO (Ex.Exceptional String [Posting]))
  -- ^ Parses a file of transactions from the financial
  -- institution. The function must open the file and parse it. This
  -- is in the IO monad not only because the function must open the
  -- file itself, but also so the function can perform arbitrary IO
  -- (run pdftotext, maybe?) If there is failure, the function can
  -- return an Exceptional String, which is the error
  -- message. Alternatively the function can raise an exception in the
  -- IO monad (currently Brenner makes no attempt to catch these) so
  -- if any of the IO functions throw you can simply not handle the
  -- exceptions.
  --
  -- The first element of the pair is a help string which should
  -- indicate how to download the data, as a helpful reminder.

  , toLincolnPayee :: Desc -> Payee -> L.Payee
  -- ^ Sometimes the financial institution provides Payee information,
  -- sometimes it does not. Sometimes the Desc might have additional
  -- information that you might want to remove. This function can be
  -- used to do that. The resulting Lincoln Payee is used for any
  -- transactions that are created by the merge command. The resulting
  -- payee is also used when comparing new financial institution
  -- postings to already existing ledger transactions in order to
  -- guess at which payee and accounts to create in the transactions
  -- created by the merge command.

  }

-- | Configuration for the Brenner program. You can optionally have
-- a default FitAcct, which is used if you do not specify any FitAcct on the
-- command line. You can also name any number of additional FitAccts. If
-- you do not specify a default FitAcct, you must specify a FitAcct on the
-- command line.

data Config = Config
  { defaultFitAcct :: Maybe (Name, FitAcct)
  , moreFitAccts :: [(Name, FitAcct)]
  }

newtype FitFileLocation = FitFileLocation { unFitFileLocation :: String }
  deriving (Show, Eq)

newtype AllowNew = AllowNew { unAllowNew :: Bool }
  deriving (Show, Eq)
