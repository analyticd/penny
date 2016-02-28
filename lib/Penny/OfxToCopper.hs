{-# LANGUAGE TemplateHaskell #-}
-- | Converts an OFX file to Copper transactions.
module Penny.OfxToCopper where

import Penny.Account
import qualified Penny.Commodity as Cy
import Penny.Copper.Types
import Penny.Polar

import qualified Control.Lens as L
import Control.Monad ((<=<))
import Data.Sequence (Seq)
import qualified Data.Sequence as Seq
import qualified Data.OFX as OFX
import Data.Text (Text)
import qualified Data.Text as X
import Data.Time (ZonedTime)
import qualified Text.Parsec as Parsec

data Error
  = OFXParseError String
  -- ^ Could not parse OFX file.
  | TransactionParseError String
  -- ^ Could not get list of transactions from file.
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
  = (return . makeCopperTransactions)
  <=< makeOfxTransactions
  <=< parseOfxFile
  where
    makeCopperTransactions
      = Seq.fromList
      . map (ofxTransactionToTransaction mainAccount getOffset commodity)
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
  -> Transaction
ofxTransactionToTransaction mainAccount getOffset commodity txn
  = Transaction tl postings
  where
    tl = TopLine'Maybe . Just . TopLine $ topLineForest
      (OFX.txDTPOSTED txn)
    postings = undefined

-- | Gets the top line forest, which contains the date and the
-- payee.
topLineForest = undefined
{-
  :: ZonedTime
  -- ^ Date transaction was posted
  -> Text
  -- ^ Payee information
  -> Forest
topLineForest = undefined
-}

