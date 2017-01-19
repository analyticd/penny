-- | Editing Copper files in place.  Typically for use in a REPL.

module Penny.Editor where

import qualified Accuerr
import Data.Foldable (toList)
import Data.Text (Text)
import qualified Data.Text as X
import qualified Data.Text.IO as XIO
import Control.Exception (throwIO)
import Data.Sequence (Seq)
import Data.Typeable (Typeable)
import Pinchot (Loc)

import Penny.Cursor
import Penny.Copper
import Penny.Copper.Tracompri
import Penny.Copper.Terminalizers
import Penny.Copper.Types
import Penny.Unix.Diff

-- | Edits at the decopperized level.  Typically this is easier, but
-- you have less control over the ultimate whitespace formatting, as
-- the default formatter is used.
editDecopperized
  :: (Show a, Typeable a)
  => (Seq (Tracompri Cursor) -> Seq (Tracompri a))
  -- ^ This function will edit each 'Tracompri'.  You will probably
  -- need a bunch of lenses to do anything useful in this function.
  -> Text
  -- ^ Filename to edit
  -> IO Patch
editDecopperized f fn = do
  tracompris <- fmap f $ parseConvertProofSingleIO fn
  wholeFile <- case copperizeAndFormat tracompris of
    Accuerr.AccFailure e -> throwIO e
    Accuerr.AccSuccess g -> return g
  let newTxt = X.pack . toList . fmap fst . t'WholeFile $ wholeFile
  diff (X.unpack fn) newTxt

-- | Edits at the copperized level.  This is typically more tedious
-- than editing at the decopperized level as 'editDecopperized' does,
-- but it gives you more control over the formatting.
editCopperized
  :: (WholeFile Char Loc -> WholeFile Char a)
  -- ^ This function will edit the 'WholeFile'
  -> Text
  -- ^ Filename to edit
  -> IO Patch
editCopperized f fn = do
  txt <- XIO.readFile (X.unpack fn)
  wholeFile <- case parseWholeFile (GivenFilename fn) txt of
    Left e -> throwIO e
    Right w -> return w
  let newTxt = X.pack . toList . fmap fst . t'WholeFile . f $ wholeFile
  diff (X.unpack fn) newTxt
