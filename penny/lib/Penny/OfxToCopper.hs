{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE OverloadedLists #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE FlexibleContexts #-}
-- | Converts an OFX file to Copper transactions.
module Penny.OfxToCopper where

import Penny.Account
import Penny.Amount (Amount)
import qualified Penny.Amount as Amount
import Penny.Arrangement
import qualified Penny.Commodity as Cy
import Penny.Copper
import Penny.Copper.Copperize
import Penny.Copper.Decopperize
import Penny.Copper.EarleyGrammar
import Penny.Copper.Freezer
import Penny.Copper.Productions
import qualified Penny.Copper.Proofer as Proofer
import Penny.Copper.Tracompri
import Penny.Copper.Types (WholeFile)
import Penny.Copper.Util
import Penny.Cursor
import Penny.Decimal
import Penny.Ents
import Penny.Polar
import qualified Penny.Fields as Fields
import Penny.Tranche (Postline)
import qualified Penny.Tranche as Tranche
import Penny.Transaction

import Accuerr (Accuerr)
import qualified Accuerr
import qualified Control.Lens as Lens
import Control.Monad ((<=<), join)
import Data.Map (Map)
import qualified Data.Map as Map
import qualified Data.OFX as OFX
import Data.Sequence (Seq)
import qualified Data.Sequence as Seq
import Data.Sequence.NonEmpty (NonEmptySeq)
import qualified Data.Sequence.NonEmpty as NE
import Data.Set (Set)
import qualified Data.Set as Set
import Data.Text (Text)
import qualified Data.Text as X
import Data.Time (ZonedTime)
import qualified Data.Time as Time
import qualified Pinchot
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
  , _arrangement :: Arrangement
  -- ^ How to arrange the foreign posting.
  } deriving Show

Lens.makeLenses ''OfxOut

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

offsettingMeta
  :: OfxOut
  -> Tranche.Postline ()
offsettingMeta out = Tranche.Tranche () [] fields
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

ofxToTxn
  :: OfxIn
  -> OfxOut
  -> Transaction ()
ofxToTxn ofxIn ofxOut = Transaction tl bal
  where
    tl = topLineTranche ofxIn ofxOut
    bal = twoPostingBalanced (rar, metaForeign) cy (_arrangement ofxOut)
      metaOffset
      where
        (metaForeign, (Amount.Amount cy q)) = foreignPair ofxIn ofxOut
        rar = repDecimal (Right Nothing) q
        metaOffset = offsettingMeta ofxOut

-- | Given an OFX Transaction, make an OfxIn.  Fails only if the
-- amount cannot be parsed.

mkOfxIn :: OFX.Transaction -> Maybe OfxIn
mkOfxIn ofxTxn = case runParser parser decTxt of
  Left _ -> Nothing
  Right copper -> Just ofxIn
    where
      dec = dDecimalRadPer copper
      ofxIn = OfxIn ofxTxn dec
  where
    parser = fmap a'DecimalRadPer earleyGrammar
    decTxt = X.pack . OFX.txTRNAMT $ ofxTxn

-- | Given an OfxIn, make a default OfxOut.
defaultOfxOut :: OfxIn -> OfxOut
defaultOfxOut ofxIn = OfxOut
  { _payee = case OFX.txPayeeInfo . _ofxTxn $ ofxIn of
      Nothing -> ""
      Just (Left str) -> X.pack str
      Just (Right pye) -> X.pack . OFX.peNAME $ pye
  , _flag = Nothing
  , _foreignAccount = []
  , _flipSign = False
  , _offsettingAccount = []
  , _commodity = ""
  , _arrangement = Arrangement CommodityOnRight True
  }

-- * Command-line program

-- | Creates a command-line program that imports OFX, creates
-- transactions, and prints them to standard output, optionally
-- appending them to the last file given on the command line.
ofxImportProgram
  :: String
  -- ^ Describe here what account you are importing from.
  -> String
  -- ^ Any additional help text you want goes here.
  -> (OfxIn -> Maybe OfxOut)
  -- ^ This function creates the necessary transaction information.
  -- If you do not want to import the transaction, return 'Nothing'.
  -- Transactions whose fitid is a duplicate also will not be
  -- imported.
  -> IO ()
ofxImportProgram = undefined

-- | Modifies a plain function to one that also returns its original
-- argument.
returnWithArgument
  :: Applicative f
  => (a -> f b)
  -> a
  -> f (a, b)
returnWithArgument f a = fmap g (f a)
  where
    g b = (a, b)

-- | Given a sequence of filenames and the text that is within those
-- files, returns a map.  The keys in this map are all the accounts,
-- and the values are a set of all the fitids.  Also, is the sequence
-- of filenames is non-empty, returns the last filename, and its
-- corresponding WholeFile.
getAccountsMap
  :: Seq (Filename, Text)
  -> Accuerr (NonEmptySeq (ParseConvertProofError Pinchot.Loc))
             (Map Account (Set Text), Maybe (Filename, WholeFile Char Pinchot.Loc))
getAccountsMap = fmap f . parseFilesWithFilenames
  where
    f sq = case Lens.unsnoc sq of
      Nothing -> (Map.empty, Nothing)
      Just (_, (fn, whole, _)) -> (acctMap, Just (fn, whole))
        where
          acctMap = foldl addTracompriToAccountMap Map.empty
            . join . fmap (Lens.view Lens._3) $ sq

parseFilesWithFilenames
  :: Seq (Filename, Text)
  -> Accuerr (NonEmptySeq (ParseConvertProofError Pinchot.Loc))
             (Seq (Filename, WholeFile Char Pinchot.Loc, Seq (Tracompri Cursor)))
parseFilesWithFilenames
  = fmap (fmap (\((fn, _), (whole, tras)) -> (fn, whole, tras)))
  . traverse (returnWithArgument parseConvertProofAccuerr)

addTracompriToAccountMap
  :: Map Account (Set Text)
  -> Tracompri a
  -> Map Account (Set Text)
addTracompriToAccountMap oldMap (Tracompri'Transaction txn)
  = foldl addPostlineToAccountMap oldMap
  . fmap snd
  . balancedToSeqEnt
  . _postings
  $ txn
addTracompriToAccountMap oldMap _ = oldMap

addPostlineToAccountMap
  :: Map Account (Set Text)
  -> Postline a
  -> Map Account (Set Text)
addPostlineToAccountMap acctMap postline
  = case Lens.view Tranche.fitid postline of
      Nothing -> acctMap
      Just fitid -> Map.alter f acct acctMap
        where
          acct = Lens.view Tranche.account postline
          f = Just . maybe (Set.singleton fitid) (Set.insert fitid)
