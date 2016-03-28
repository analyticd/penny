{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE OverloadedLists #-}
-- | Converts an OFX file to Copper transactions.
module Penny.OfxToCopper where

import Penny.Account
import qualified Penny.Commodity as Cy
import Penny.Copper.Char
import Penny.Copper.DateTime
import Penny.Copper.Types
import Penny.Copper.Util
import Penny.Copper.Formatted
import Penny.Copper.DateTime
import Penny.Polar

import qualified Control.Lens as L
import Control.Monad ((<=<))
import Data.Sequence (Seq)
import qualified Data.Sequence as Seq
import qualified Data.OFX as OFX
import Data.Text (Text)
import qualified Data.Text as X
import Data.Time (ZonedTime)
import qualified Data.Time as Time
import qualified Text.Parsec as Parsec

data Error
  = OFXParseError String
  -- ^ Could not parse OFX file.
  | DateConvertError Time.Day
  | ZonedTimeConvertError Time.ZonedTime
  | TransactionParseError String
  -- ^ Could not get list of transactions from file.
  | NoPayee
  deriving Show

-- | Every OFX transaction must create two postings in the Copper
-- file.  One of these postings is called the main posting, and it
-- is in the account that is the subject of the OFX file.  For
-- example, if the OFX file contains Amex OFX transactions, the main
-- account might be @[Liabilities, Amex]@.
--
-- The other account is the offsetting account.  It will vary from
-- one OFX transaction to another.  For example, a charge for
-- gasoline might have an account for @[Expenses, Auto, Gas]@, while
-- a payment might have an account of @[Assets, Checking]@.  The
-- information needed to create an offset is stored in an 'Offset'.
-- 
-- The offsetting account always has a non-empty Trio.  The main
-- accont has no Trio.

data Offset = Offset
  { _offsettingAccount :: Account
  , _offsettingPole :: Pole
  , _offsettingPayee :: Text
  } deriving Show

L.makeLenses ''Offset

-- | Function that is applied to determine the 'Offset' for a
-- particular OFX transaction.
type GetOffset
  = Pole
  -> Maybe (Either String OFX.Payee)
  -> OFX.TrnType
  -> Offset

-- | Parses OFX file using Parsec parsers, gets the OFX
-- transactions, and converts them to Copper transactions.
ofxTextToTransactions
  :: Account
  -- ^ Place main transaction into this account.
  -> GetOffset
  -- ^ Determines offsetting information
  -> Cy.Commodity
  -- ^ All postings have this commodity.
  -> Text
  -> Either Error (Seq Transaction)
ofxTextToTransactions mainAccount getOffset commodity
  = makeCopperTransactions
  <=< makeOfxTransactions
  <=< parseOfxFile
  where
    makeCopperTransactions
      = fmap Seq.fromList
      . traverse (ofxTransactionToTransaction mainAccount getOffset commodity)
    makeOfxTransactions
      = either (Left . TransactionParseError) Right
      . OFX.transactions
    parseOfxFile
      = either (Left . OFXParseError . show) Right
      . Parsec.parse OFX.ofxFile ""
      . X.unpack

ofxTransactionToTransaction
  :: Account
  -- ^ Place transaction into this account.
  -> GetOffset
  -- ^ Determines offsetting information
  -> Cy.Commodity
  -- ^ All postings have this commodity.
  -> OFX.Transaction
  -> Either Error Transaction
ofxTransactionToTransaction mainAccount getOffset commodity txn
  = Transaction
  <$> getTopLine (OFX.txDTPOSTED txn) (OFX.txPayeeInfo txn)
  <*> getPostings mainAccount getOffset commodity
        (OFX.txFITID txn) (OFX.txTRNAMT txn)

getTopLine
  :: Time.ZonedTime
  -> Maybe (Either String OFX.Payee)
  -> Either Error TopLine'Maybe
getTopLine zonedTime payee = do
  (date, time, zone) <- maybe (Left . ZonedTimeConvertError $ zonedTime)
    Right . c'Copper'ZonedTime $ zonedTime
  payeeT <- payeeTree payee
  return . TopLine'Maybe . Just . TopLine $ fForest
    (spinster $ Scalar'Date date)
    [ spinster $ Scalar'Time time
    , spinster $ Scalar'Zone zone
    , payeeT ]

payeeTree :: Maybe (Either String OFX.Payee) -> Either Error Tree
payeeTree = fmap toTree . maybe (Left NoPayee) Right
  where
    toTree = makeTree . either id OFX.peNAME
      where
        makeTree str = fTree sc Nothing
          where
            sc = case fString (Seq.fromList str) of
              Left us -> Scalar'UnquotedString us
              Right qs -> Scalar'QuotedString qs

getPostings
  :: Account
  -> GetOffset
  -> Cy.Commodity
  -> String
  -- ^ FITID
  -> String
  -- ^ Transaction amount
  -> Either Error Postings
getPostings account getOffset cy fitid amt = undefined

getMainPosting
  :: Account
  -- ^ Place main posting into this account
  -> Pole
  -- ^ Pole for the main posting
  -> String
  -- ^ FITID
  -> String
  -- ^ Transaction amount
  -> Either Error Posting
getMainPosting = undefined
