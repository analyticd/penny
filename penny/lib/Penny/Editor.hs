{-# LANGUAGE NoImplicitPrelude #-}
-- | Editing Copper files in place.  Typically for use in a REPL.

module Penny.Editor where

import Penny.Cursor
import Penny.Copper
import Penny.Copper.Tracompri
import Penny.Copper.Terminalizers
import Penny.Copper.Types
import Penny.Prelude
import Penny.Unix.Diff

-- | Edits at the decopperized level.  Typically this is easier, but
-- you have less control over the ultimate whitespace formatting, as
-- the default formatter is used.
editDecopperized
  :: (Show a, Typeable a)
  => (Seq (Tracompri Cursor) -> Seq (Tracompri a))
  -- ^ This function will edit each 'Tracompri'.  You will probably
  -- need a bunch of lenses to do anything useful in this function.
  -> FilePath
  -- ^ Filename to edit
  -> IO Patch
editDecopperized f fn = do
  tracompris <- fmap f $ parseConvertProofSingleIO fn
  wholeFile <- case copperizeAndFormat tracompris of
    AccFailure e -> throwIO e
    AccSuccess g -> return g
  let newTxt = pack . toList . fmap fst . t'WholeFile $ wholeFile
  diff fn newTxt

-- | Edits at the copperized level.  This is typically more tedious
-- than editing at the decopperized level as 'editDecopperized' does,
-- but it gives you more control over the formatting.
editCopperized
  :: (WholeFile Char Loc -> WholeFile Char a)
  -- ^ This function will edit the 'WholeFile'
  -> FilePath
  -- ^ Filename to edit
  -> IO Patch
editCopperized f fn = do
  txt <- readFile fn
  wholeFile <- case parseWholeFile (Right fn) txt of
    Left e -> throwIO e
    Right w -> return w
  let newTxt = pack . toList . fmap fst . t'WholeFile . f $ wholeFile
  diff fn newTxt
