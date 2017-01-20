{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE NoImplicitPrelude #-}
-- | Given a Copper files, change every Cleared posting to
-- a Reconciled one.

module Penny.Reconciler where

import qualified Control.Exception as Exception
import qualified Control.Lens as Lens
import qualified Data.Text as X

import Penny.Copper
import Penny.Copper.Copperize (TracompriError)
import Penny.Copper.Terminalizers
import Penny.Copper.Tracompri
import Penny.Cursor
import Penny.Prelude
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
  copperInput <- readFile filename
  tracompris <- either Exception.throwIO return
    $ reconcileFile (filename, copperInput)
  formatted <- either (Exception.throwIO . CopperizationFailed) return
    . Lens.over Lens._Left  (\(CopperizationError a) -> a)
    . accuerrToEither
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
  :: (FilePath, Text)
  -> Either ReconcilerFailure (Seq (Tracompri Cursor))
reconcileFile copperInput = do
  tracompris <- Lens.over Lens._Left ParseConvertProofFailed
    . parseConvertProof
    . Lens.over Lens._1 Right
    $ copperInput
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
