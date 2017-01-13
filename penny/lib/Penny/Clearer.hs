{-# LANGUAGE OverloadedStrings #-}
-- | Given an OFX file, change every matching posting that does not
-- currently have a flag to Cleared.  Results are printed to standard
-- output.
module Penny.Clearer where

import qualified Accuerr
import qualified Control.Lens as Lens
import Data.Foldable (toList)
import qualified Data.OFX as OFX
import Data.Sequence (Seq)
import Data.Set (Set)
import qualified Data.Set as Set
import Data.Text (Text)
import qualified Data.Text as X
import qualified Data.Text.IO as XIO
import Pinchot (Loc)

import Penny.Account
import Penny.Copper
import Penny.Copper.Terminalizers (t'WholeFile)
import Penny.Copper.Tracompri
import Penny.Cursor
import Penny.Fields
import Penny.Tranche (fields)
import Penny.Transaction
import Penny.Unix

clearer
  :: Account
  -- ^ Clear this account
  -> Text
  -- ^ Read this OFX file
  -> Text
  -- ^ Read this Copper file
  -> IO ()
  -- ^ Clears the Copper file.  Destructively writes, so use a VCS.
clearer acct cmdLine copperFile = do
  ofxTxt <- XIO.readFile (X.unpack cmdLine)
  copperInput <- XIO.readFile (X.unpack copperFile)
  tracompris <- errorExit $ clearFile acct ofxTxt
    (GivenFilename copperInput, copperFile)
  formatted <- errorExit . Accuerr.accuerrToEither . copperizeAndFormat
    $ tracompris
  let result = X.pack . toList . fmap fst . t'WholeFile $ formatted
  XIO.writeFile (X.unpack copperFile) result

data ClearerFailure
  = ParseConvertProofFailed (ParseConvertProofError Loc)
  | OfxImportFailed String
  deriving Show

-- | Given the input OFX file and the input Penny file, create the result.
clearFile
  :: Account
  -- ^ Clear postings only if they are in this account
  -> Text
  -- ^ OFX file
  -> (InputFilespec, Text)
  -- ^ Copper file
  -> Either ClearerFailure (Seq (Tracompri Cursor))
clearFile acct ofxTxt copperInput = do
  tracompris <- Lens.over Lens._Left ParseConvertProofFailed
    . parseConvertProof $ copperInput
  ofxTxns <- Lens.over Lens._Left OfxImportFailed
    . OFX.parseTransactions . X.unpack $ ofxTxt
  let fitids = allFitids ofxTxns
  return (Lens.over traversePostingFields (clearPosting acct fitids) tracompris)

-- | Gets a set of all fitid from a set of OFX transactions.
allFitids
  :: [OFX.Transaction]
  -> Set Text
allFitids = foldr g Set.empty
  where
    g txn = Set.insert (X.pack $ OFX.txFITID txn)

-- | Given a single posting and a set of all fitid, modify the posting
-- to Cleared if it currently does not have a flag.
clearPosting
  :: Account
  -- ^ Only clear the posting if it is in this account.
  -> Set Text
  -- ^ All fitid.  Clear the posting only if its fitid is in this set.
  -> PostingFields
  -> PostingFields
clearPosting acct fitids pf
  | acctMatches && fitIdFound && noCurrentFlag = Lens.set flag "C" pf
  | otherwise = pf
  where
    acctMatches = Lens.view account pf == acct
    fitIdFound = Set.member (Lens.view fitid pf) fitids
    noCurrentFlag = Lens.view flag pf == ""

traversePostingFields
  :: Lens.Traversal' (Seq (Tracompri a)) PostingFields
traversePostingFields
  = traverse
  . _Tracompri'Transaction
  . postings
  . traverse
  . fields
