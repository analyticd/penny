{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE OverloadedLists #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE FlexibleContexts #-}
-- | Converts an OFX file to Copper transactions.
--
-- Typically all you wil need is 'ofxImportProgram' and the types it
-- uses as its arguments.  Use 'ofxImportProgram' as your @Main.main@
-- function, as it will handle all command-line parsing.
--
-- Some terminology:
--
-- [@foreign posting@] A Penny posting corresponding to the posting on
-- the financial institution statement.  For example, if importing an
-- OFX from a checking account, this posting might have the account
-- @[\"Assets\", \"Checking\"]@.
--
-- [@offsetting posting@] A Penny posting whose amount offsets the
-- foreign posting.  For example, if importing an OFX from a checking
-- account, this posting might have the account
-- @[\"Expenses\", \"Bank Fees\"]@.
module Penny.OfxToCopper where

import Penny.Account
import Penny.Amount (Amount)
import qualified Penny.Amount as Amount
import Penny.Arrangement
import qualified Penny.Commodity as Cy
import Penny.Copper
  (ParseConvertProofError, parseProduction, parseConvertProofFiles)
import Penny.Copper.Copperize
import Penny.Copper.Decopperize
import Penny.Copper.Formatter
import Penny.Copper.Productions
import Penny.Copper.Terminalizers (t'WholeFile)
import Penny.Copper.Tracompri
import Penny.Copper.Types (WholeFile)
import Penny.Cursor
import Penny.Decimal
import Penny.Ents
import qualified Penny.Fields as Fields
import Penny.SeqUtil
import Penny.Tranche (Postline)
import qualified Penny.Tranche as Tranche
import Penny.Transaction
import Penny.Unix

import Accuerr (Accuerr)
import qualified Accuerr
import Control.Applicative (many)
import qualified Control.Lens as Lens
import Control.Monad (join, guard)
import Data.Foldable (toList)
import Data.Map (Map)
import qualified Data.Map as Map
import Data.Monoid ((<>))
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
import qualified Options.Applicative as A
import qualified Pinchot

-- * Data types

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
  , _zonedTime :: ZonedTime
  } deriving Show

Lens.makeLenses ''OfxOut

-- * Helpers for dealing with OFX data

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
  , _zonedTime = OFX.txDTPOSTED . _ofxTxn $ ofxIn
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
ofxImportProgram account addlHelp modOfxText
  = A.execParser opts >>= runCommandLineProgram modOfxText
  where
    opts = A.info (A.helper <*> commandLine)
      ( A.fullDesc
        <> A.progDesc ("import OFX transactions for the " ++
           show account ++ " account.")
        <> A.footer addlHelp)

-- | Contains all command-line options.
data CommandLine = CommandLine
  { _ofxFilenames :: Seq Text
  -- ^ These are the filenames of the OFX files the user wants to read.
  , _copperFiles :: Seq Text
  -- ^ These are the Copper files the user wants to read.
  } deriving Show

-- | An @optparse-applicative@ parser for 'CommandLine'.
commandLine :: A.Parser CommandLine
commandLine = CommandLine
  <$> fmap (Seq.fromList . fmap X.pack)
        (many (A.argument A.str (A.metavar "OFX FILE")))
  <*> fmap (fmap X.pack . Seq.fromList) (many copperFile)
  where
    copperFile = A.strOption (A.long "file"
        <> A.short 'f'
        <> A.help "Load existing transactions from this Copper file"
        <> A.metavar "FILENAME")

-- | Given the parsed 'CommandLine', run the command-line program.
runCommandLineProgram
  :: (OfxIn -> Maybe OfxOut)
  -- ^
  -> CommandLine
  -- ^
  -> IO ()
runCommandLineProgram fOfx cmdLine = do
  readCopperFiles <- readCommandLineFiles . _copperFiles $ cmdLine
  readOfxFiles <- fmap (fmap snd)
    . readCommandLineFiles . _ofxFilenames $ cmdLine
  let readStrings = NE.nonEmptySeqToSeq . fmap X.unpack $ readOfxFiles
  result <- errorExit $ ofxImportWithCopperParse fOfx
    (NE.nonEmptySeqToSeq readCopperFiles)
    readStrings
  printResult result

-- | Prints the 'WholeFile' to standard output.
printResult :: WholeFile Char () -> IO ()
printResult = putStr . toList . fmap fst . t'WholeFile

-- * Errors

type Accuseq a = Accuerr (NonEmptySeq a)

data OfxToCopperError
  = OTCBadOfxFile String
  | OTCBadOfxAmount (NonEmptySeq OFX.Transaction)
  | OTCBadCopperFile (NonEmptySeq (ParseConvertProofError Pinchot.Loc))
  | OTCCopperizationFailed (NonEmptySeq (TracompriError ()))
  deriving Show

-- * Creating Penny transactions

-- | Given OFX data, creates a 'Tranche.TopLine'.
topLineTranche
  :: OfxIn
  -- ^
  -> OfxOut
  -- ^
  -> Tranche.TopLine ()
topLineTranche inp out = Tranche.Tranche ()
  $ Fields.TopLineFields zt pye origPye
  where
    zt = _zonedTime out
    pye = _payee out
    origPye = case OFX.txPayeeInfo . _ofxTxn $ inp of
      Nothing -> X.empty
      Just (Left s) -> X.pack s
      Just (Right p) -> X.pack . OFX.peNAME $ p

-- | Given OFX data, create the foreign posting.
foreignPair
  :: OfxIn
  -- ^
  -> OfxOut
  -- ^
  -> (Tranche.Postline (), Amount)
foreignPair inp out = (Tranche.Tranche () fields, amt)
  where
    fields = Fields.PostingFields
      { Fields._number = Nothing
      , Fields._flag = toFlag . _flag $ out
      , Fields._account = _foreignAccount out
      , Fields._fitid = X.pack . OFX.txFITID . _ofxTxn $ inp
      , Fields._tags = Seq.empty
      , Fields._uid = X.empty
      , Fields._trnType = Just . OFX.txTRNTYPE . _ofxTxn $ inp
      , Fields._origDate = Just . OFX.txDTPOSTED . _ofxTxn $ inp
      , Fields._memo = Seq.empty
      }
      where
        toFlag Nothing = ""
        toFlag (Just Cleared) = "C"
        toFlag (Just Reconciled) = "R"
    amt = Amount.Amount
      { Amount._commodity = _commodity out
      , Amount._qty = flipper . _qty $ inp
      }
      where
        flipper | _flipSign out = negate
                | otherwise = id

-- | Given the OFX data, create the 'Tranche.Postline' for the
-- offsetting posting.  The offsetting posting does not get an amount
-- because ultimately we use 'twoPostingBalanced', which does not
-- require an amount for the offsetting posting.
offsettingMeta
  :: OfxOut
  -- ^
  -> Tranche.Postline ()
  -- ^
offsettingMeta out = Tranche.Tranche () fields
  where
    fields = Fields.PostingFields
      { Fields._number = Nothing
      , Fields._flag = X.empty
      , Fields._account = _offsettingAccount out
      , Fields._fitid = X.empty
      , Fields._tags = Seq.empty
      , Fields._uid = X.empty
      , Fields._trnType = Nothing
      , Fields._origDate = Nothing
      , Fields._memo = Seq.empty
      }

-- | Given the OFX data, create a 'Transaction'.
ofxToTxn
  :: OfxIn
  -- ^
  -> OfxOut
  -- ^
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

-- * Parsing Copper data and importing OFX data

-- | Importing OFX transactions is a three-step process:
--
-- 1. Parse Copper files.  These files contain existing transactions and
-- are used to detect duplicate OFX transactions.  Also, optionally
-- the new OFX transactions are appended to the last Copper file, so
-- the parse step returns the last Copper file.
--
-- 2. Import OFX transactions.  Uses the duplicate detector from step 1.
--
-- 3. Copperize and format new transactions.  Append new transactions
-- to Copper file, if requested.  Return new, formatted transactions,
-- and new Copper file (if requested).
ofxImportWithCopperParse
  :: (OfxIn -> Maybe OfxOut)
  -- ^ Examines each OFX transaction
  -> Seq (InputFilespec, Text)
  -- ^ Copper files with their names
  -> Seq String
  -- ^ OFX input files
  -> Either OfxToCopperError (WholeFile Char ())
  -- ^ If successful, returns the formatted new transactions.  Also,
  -- if there is a last Copper file, returns its filename and its
  -- parsed 'WholeFile'.
ofxImportWithCopperParse fOrig copperFiles ofxFiles = do
  fNoDupes <- Lens.over Lens._Left OTCBadCopperFile
    . parseCopperFiles fOrig $ copperFiles
  txns <- fmap join . traverse (importOfxTxns fNoDupes) $ ofxFiles
  Lens.over Lens._Left OTCCopperizationFailed
    . copperizeAndFormat $ txns


-- * Importing OFX data (without Copper data)

-- | Imports all OFX transactions.
importOfxTxns
  :: (OfxIn -> Maybe OfxOut)
  -- ^ Examines each OFX transaction.  This function must do something
  -- about duplicates, as 'importOfxTxns' does nothing special to
  -- handle duplicates.
  -> String
  -- ^ OFX input file
  -> Either OfxToCopperError (Seq (Transaction ()))
importOfxTxns fOfx inp = do
  ofx <- Lens.over Lens._Left OTCBadOfxFile
    . OFX.parseTransactions $ inp
  let ofxMkr t = case mkOfxIn t of
        Nothing -> Accuerr.AccFailure $ NE.singleton t
        Just r -> Accuerr.AccSuccess r
  ofxIns <- case traverse ofxMkr ofx of
    Accuerr.AccFailure fails -> Left (OTCBadOfxAmount fails)
    Accuerr.AccSuccess g -> Right . Seq.fromList $ g
  let maybeOfxOuts = fmap fOfx ofxIns
  return . fmap (uncurry ofxToTxn) . Seq.zip ofxIns . catMaybes
    $ maybeOfxOuts

-- | Given an OFX Transaction, make an OfxIn.  Fails only if the
-- amount cannot be parsed.

mkOfxIn :: OFX.Transaction -> Maybe OfxIn
mkOfxIn ofxTxn = case parseProduction a'DecimalRadPer decTxt of
  Left _ -> Nothing
  Right copper -> Just ofxIn
    where
      dec = dDecimalRadPer copper
      ofxIn = OfxIn ofxTxn dec
  where
    decTxt = X.pack . OFX.txTRNAMT $ ofxTxn

-- | Takes an OfxIn and imports it to Transaction.  Rejects
-- duplicates.
importOfx
  :: Map Account (Set Text)
  -- ^
  -> (OfxIn -> Maybe OfxOut)
  -- ^
  -> OfxIn
  -- ^
  -> Maybe OfxOut
importOfx lkp f inp = do
  out <- f inp
  guard (freshPosting inp out lkp)
  return out


-- * Reading Copper data

-- | Parses existing copper files.  Takes a function that creates new
-- transactions, and returns a new function that will also reject
-- duplicates.
parseCopperFiles
  :: (OfxIn -> Maybe OfxOut)
  -- ^ Original function
  -> Seq (InputFilespec, Text)
  -> Either (NonEmptySeq (ParseConvertProofError Pinchot.Loc))
            (OfxIn -> Maybe OfxOut)
parseCopperFiles fOrig
  = fmap f . Lens.view Accuerr.isoAccuerrEither . getAccountsMap
  where
    f acctMap = importOfx acctMap fOrig


-- * Formatting Copperized data

-- | Returns a 'WholeFile' that is suitable for use as a standalone
-- file.  There is no extra whitespace at the beginning or end of the
-- text.
copperizeAndFormat
  :: Seq (Transaction ())
  -- ^
  -> Either (NonEmptySeq (TracompriError ())) (WholeFile Char ())
copperizeAndFormat
  = fmap (formatWholeFile 4)
  . Accuerr.accuerrToEither
  . tracompriWholeFile
  . fmap Tracompri'Transaction


-- * Duplicate detection

-- | Returns True if this transaction is not a duplicate.
freshPosting
  :: OfxIn
  -- ^
  -> OfxOut
  -- ^
  -> Map Account (Set Text)
  -- ^
  -> Bool
freshPosting inp out lkp = case Map.lookup acctName lkp of
  Nothing -> True
  Just set -> not $ Set.member fitid set
  where
    acctName = _foreignAccount out
    fitid = X.pack . OFX.txFITID . _ofxTxn $ inp


-- | Given a sequence of filenames and the text that is within those
-- files, returns a map.  The keys in this map are all the accounts,
-- and the values are a set of all the fitids.  Also, if the sequence
-- of filenames is non-empty, returns the last filename, and its
-- corresponding WholeFile.
getAccountsMap
  :: Seq (InputFilespec, Text)
  -- ^
  -> Accuerr (NonEmptySeq (ParseConvertProofError Pinchot.Loc))
             (Map Account (Set Text))
  -- ^
getAccountsMap = fmap f . parseConvertProofFiles
  where
    f = foldl addTracompriToAccountMap Map.empty . join

addTracompriToAccountMap
  :: Map Account (Set Text)
  -- ^
  -> Tracompri a
  -- ^
  -> Map Account (Set Text)
  -- ^
addTracompriToAccountMap oldMap (Tracompri'Transaction txn)
  = foldl addPostlineToAccountMap oldMap
  . fmap snd
  . balancedToSeqEnt
  . _postings
  $ txn
addTracompriToAccountMap oldMap _ = oldMap

addPostlineToAccountMap
  :: Map Account (Set Text)
  -- ^
  -> Postline a
  -- ^
  -> Map Account (Set Text)
  -- ^
addPostlineToAccountMap acctMap postline
  | X.null fitid = acctMap
  | otherwise = Map.alter f acct acctMap
  where
    fitid = Lens.view Tranche.fitid postline
    acct = Lens.view Tranche.account postline
    f = Just . maybe (Set.singleton fitid) (Set.insert fitid)
