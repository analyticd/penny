{-# LANGUAGE OverloadedStrings #-}
-- | Given a Copper files, change every Cleared posting to
-- a Reconciled one.

module Penny.Reconciler where

import qualified Accuerr
import qualified Control.Exception as Exception
import qualified Control.Lens as Lens
import Data.Foldable (toList)
import Data.Sequence (Seq)
import Data.Sequence.NonEmpty (NonEmptySeq)
import Data.Text (Text)
import qualified Data.Text as X
import qualified Data.Text.IO as XIO
import Pinchot (Loc)

import Penny.Copper
import Penny.Copper.Copperize (TracompriError)
import Penny.Copper.Terminalizers
import Penny.Copper.Tracompri
import Penny.Cursor
import Penny.Tranche
import Penny.Transaction
import Penny.Unix.Diff

-- | Reconciles a file by changing all postings with a @C@ flag to an
-- @R@ flag.  Makes no changes on disk; returns result as the output of @diff@.
reconciler
  :: FilePath
  -- ^ Reconcile this file
  -> IO Patch
reconciler filename = do
  copperInput <- XIO.readFile filename
  tracompris <- either Exception.throwIO return $ reconcileFile
    (GivenFilename $ X.pack filename, copperInput)
  formatted <- either (Exception.throwIO . CopperizationFailed) return
    . Lens.over Lens._Left  (\(CopperizationError a) -> a)
    . Accuerr.accuerrToEither
    . copperizeAndFormat
    $ tracompris
  let result = X.pack . toList . fmap fst . t'WholeFile $ formatted
  diff filename result

data ReconcilerFailure
  = ParseConvertProofFailed (ParseConvertProofError Loc)
  | CopperizationFailed (NonEmptySeq (TracompriError Cursor))
  deriving Show

instance Exception.Exception ReconcilerFailure

reconcileFile
  :: (InputFilespec, Text)
  -> Either ReconcilerFailure (Seq (Tracompri Cursor))
reconcileFile copperInput = do
  tracompris <- Lens.over Lens._Left ParseConvertProofFailed
    . parseConvertProof $ copperInput
  return (Lens.over traverseFlags reconcileFlag tracompris)

-- | Marks a flag as @R@ if it is presently @C@.
reconcileFlag :: Text -> Text
reconcileFlag x
  | x == "C" = "R"
  | otherwise = x

traverseFlags :: Lens.Traversal' (Seq (Tracompri a)) Text
traverseFlags
  = traverse
  . _Tracompri'Transaction
  . postings
  . traverse
  . flag
