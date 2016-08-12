{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE OverloadedLists #-}
{-# LANGUAGE OverloadedStrings #-}
-- | Converts an OFX file to Copper transactions.
module Penny.OfxToCopper where

import Penny.Account
import Penny.Amount (Amount)
import qualified Penny.Amount as Amount
import qualified Penny.Commodity as Cy
import Penny.Copper
import Penny.Copper.EarleyGrammar
import Penny.Copper.Freezer
import Penny.Copper.Productions
import Penny.Copper.Types
import Penny.Copper.Util
import Penny.Decimal
import Penny.Polar
import qualified Penny.Fields as Fields
import qualified Penny.Tranche as Tranche

import Accuerr (Accuerr)
import qualified Control.Lens as Lens
import Control.Monad ((<=<))
import Data.Sequence (Seq)
import qualified Data.Sequence as Seq
import qualified Data.OFX as OFX
import Data.Text (Text)
import qualified Data.Text as X
import Data.Time (ZonedTime)
import qualified Data.Time as Time
import qualified Text.Parsec as Parsec

-- | Inputs from the OFX file.
data OfxIn = OfxIn
  { _ofxTxn :: OFX.Transaction
  , _qty :: Decimal
  }

Lens.makeLenses ''OfxIn

data Flag = Cleared | Reconciled
  deriving (Eq, Ord, Show)

-- | Data used to create the resulting Penny transaction.
data OfxOut = OfxOut
  { _payee :: Text
  -- ^ Payee.  This applies to both postings.
  , _flag :: Maybe Flag
  -- ^ Whether the foreign posting is cleared, reconciled, or neither.
  -- This applies only to the foreign posting.
  , _foreignAccount :: Account
  -- ^ Account for the foreign posting.
  , _flipSign :: Bool
  -- ^ If this is False, the amount from the OFX transaction is copied
  -- to the foreign posting as-is.  If this is True, the amount from the
  -- OFX transaction is copied to the foreign posting, but its sign is
  -- reversed (so that a debit becomes a credit, and vice versa.)
  -- Arbitrarily (as set forth in "Penny.Polar", a debit is equivalent
  -- to a positive, and a credit is equal to a negative.  For example,
  -- if the OFX file is from a credit card, typically charges are
  -- positive, which in Penny corresponds to a debit.  However,
  -- typically in Penny you will record charges in a liability
  -- account, where they would be credits.  So in such a case you
  -- would use 'True' here.
  , _offsettingAccount :: Account
  -- ^ Account to use for the offsetting posting.
  , _commodity :: Cy.Commodity
  -- ^ Commodity.  Used for both the foreign and offsetting posting.
  } deriving Show

Lens.makeLenses ''OfxOut

{-
-- | Creates a new transaction ready for "Penny.Copper.Freezer".
ofxToTxnParts
  :: OfxIn
  -> OfxOut
  -> TxnParts
ofxToTxnParts inp out = TxnParts topLine pstgs
  where
    topLine = topLineTranche inp out
    pstgs = [foreignPair inp out, offsettingPair inp out]

topLineTranche
  :: OfxIn
  -> OfxOut
  -> Tranche.TopLine ()
topLineTranche inp out = Tranche.Tranche () []
  $ Fields.TopLineFields zt pye origPye
  where
    zt = OFX.txDTPOSTED . _ofxTxn $ inp
    pye = Just . _payee $ out
    origPye = case OFX.txPayeeInfo . _ofxTxn $ inp of
      Nothing -> Nothing
      Just (Left s) -> Just . X.pack $ s
      Just (Right p) -> Just . X.pack . OFX.peNAME $ p

foreignPair
  :: OfxIn
  -> OfxOut
  -> (Tranche.Postline (), Amount)
foreignPair inp out = (Tranche.Tranche () [] fields, amt)
  where
    fields = Fields.PostingFields
      { Fields._number = Nothing
      , Fields._flag = fmap toFlag . _flag $ out
      , Fields._account = _foreignAccount out
      , Fields._fitid = Just . X.pack . OFX.txFITID . _ofxTxn $ inp
      , Fields._tags = Seq.empty
      , Fields._uid = Nothing
      , Fields._trnType = Just . OFX.txTRNTYPE . _ofxTxn $ inp
      , Fields._origDay = Just . Time.localDay . Time.zonedTimeToLocalTime
          . OFX.txDTPOSTED . _ofxTxn $ inp
      , Fields._origTime = Just . Time.localTimeOfDay
          . Time.zonedTimeToLocalTime . OFX.txDTPOSTED . _ofxTxn $ inp
      , Fields._origZone = Just . Time.timeZoneMinutes . Time.zonedTimeZone
          . OFX.txDTPOSTED . _ofxTxn $ inp
      }
      where
        toFlag Cleared = "C"
        toFlag Reconciled = "R"
    amt = Amount.Amount
      { Amount._commodity = _commodity out
      , Amount._qty = flipper . _qty $ inp
      }
      where
        flipper | _flipSign out = negate
                | otherwise = id

offsettingPair
  :: OfxIn
  -> OfxOut
  -> (Tranche.Postline (), Amount)
offsettingPair inp out = (Tranche.Tranche () [] fields, amt)
  where
    fields = Fields.PostingFields
      { Fields._number = Nothing
      , Fields._flag = Nothing
      , Fields._account = _offsettingAccount out
      , Fields._fitid = Nothing
      , Fields._tags = Seq.empty
      , Fields._uid = Nothing
      , Fields._trnType = Nothing
      , Fields._origDay = Nothing
      , Fields._origTime = Nothing
      , Fields._origZone = Nothing
      }
    amt = Amount.Amount
      { Amount._commodity = _commodity out
      , Amount._qty = flipper . _qty $ inp
      }
      where
        flipper | not $ _flipSign out = negate
                | otherwise = id

-}
