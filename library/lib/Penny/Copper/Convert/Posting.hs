module Penny.Copper.Convert.Posting where

import Penny.Copper.Tree.Posting
import Penny.Copper.Tree.Memo.Posting
import Data.Sequence
import qualified Data.Sequence as S
import Penny.Numbers.Natural
import Penny.Copper.Tree.Flag
import Penny.Copper.Tree.Number
import Penny.Copper.Tree.Payee.Posting
import qualified Penny.Copper.Tree.Account.Unquoted as AU
import qualified Penny.Copper.Tree.Account.Quoted as AQ
import Penny.Copper.Tree.Tag
import Penny.Copper.Tree.Side
import Penny.Copper.Tree.Commodity
import Penny.Copper.Tree.Currency
import Penny.Copper.Tree.Amount

-- | Used to scan a sequence of Item.

data ScanAcc = ScanAcc
  { scFlag :: Maybe Flag
  , scNumber :: Maybe Number
  , scPayee :: Maybe Payee
  , scAccount :: Maybe (Either AU.Account AQ.Account)
  , scTags :: Seq Tag
  , scDrCr :: Maybe (Either Debit Credit)
  , scCommodity :: Maybe (Either Commodity Currency)
  , scAmount :: Maybe (Either AmountPeriod AmountComma)
  } deriving (Eq, Ord, Show)

emptyScanAcc :: ScanAcc
emptyScanAcc = ScanAcc Nothing Nothing Nothing Nothing S.empty Nothing Nothing Nothing

data Error
  = AlreadyFlag
  | AlreadyNumber
  | AlreadyPayee
  | AlreadyAccount
  | AlreadyDrCr
  | AlreadyCommodity
  | AlreadyAmount
  deriving (Eq, Ord, Show)

scanItem :: ScanAcc -> Item -> Either Error ScanAcc
scanItem c i = case i of
  I0 fl -> maybe (Right (c { scFlag = Just fl }))
    (const (Left AlreadyFlag)) (scFlag c)

  I1 nu -> maybe (Right (c { scNumber = Just nu}))
    (const (Left AlreadyNumber)) (scNumber c)

  I2 pa -> maybe (Right (c { scPayee = Just pa }))
    (const (Left AlreadyPayee)) (scPayee c)

  I3 ac -> maybe (Right (c { scAccount = Just (Left ac)}))
    (const (Left AlreadyAccount)) (scAccount c)

  I4 ac -> maybe (Right (c { scAccount = Just (Right ac)}))
    (const (Left AlreadyAccount)) (scAccount c)

  I5 ta -> Right ( c { scTags = scTags c |> ta })
  I6 dr -> maybe (Right (c { scDrCr = Just (Left dr)}))
    (const (Left AlreadyDrCr)) (scDrCr c)

  I7 cr -> maybe (Right (c { scDrCr = Just (Right cr)}))
    (const (Left AlreadyDrCr)) (scDrCr c)

  I8 cy -> maybe (Right (c { scCommodity = Just (Left cy)}))
    (const (Left AlreadyCommodity)) (scCommodity c)

  I9 cu -> maybe (Right (c { scCommodity = Just (Right cu)}))
    (const (Left AlreadyCommodity)) (scCommodity c)

  I10 amp -> maybe (Right (c { scAmount = Just (Left amp)}))
    (const (Left AlreadyAmount)) (scAmount c)

  I11 ac -> maybe (Right (c { scAmount = Just (Right ac)}))
    (const (Left AlreadyAmount)) (scAmount c)
