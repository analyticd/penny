{-# LANGUAGE OverloadedStrings #-}
-- | Given a Copper files, change every Cleared posting to
-- a Reconciled one.

module Penny.Reconciler where

import qualified Accuerr
import Control.Applicative (optional)
import qualified Control.Lens as Lens
import Data.Foldable (toList)
import Data.Sequence (Seq)
import Data.Text (Text)
import qualified Data.Text as X
import qualified Options.Applicative as A
import Pinchot (Loc)

import Penny.Copper
import Penny.Copper.Terminalizers
import Penny.Copper.Tracompri
import Penny.Cursor
import Penny.Tranche
import Penny.Transaction
import Penny.Unix

data CommandLine = CommandLine { _copperFile :: Maybe FilePath }
  deriving Show

commandLine :: A.Parser CommandLine
commandLine = CommandLine
  <$> optional (A.argument A.str (A.metavar "Copper file"))

runCommandLineProgram :: CommandLine -> IO ()
runCommandLineProgram cmdLine = do
  copperInput <- readMaybeCommandLineFile . fmap X.pack
    . _copperFile $ cmdLine
  tracompris <- errorExit $ reconcileFile copperInput
  formatted <- errorExit . Accuerr.accuerrToEither . copperizeAndFormat
    $ tracompris
  putStr . toList . fmap fst . t'WholeFile $ formatted

data ReconcilerFailure
  = ParseConvertProofFailed (ParseConvertProofError Loc)
  deriving Show

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
