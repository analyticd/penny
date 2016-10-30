-- | Given an OFX file, change every matching posting that does not
-- currently have a flag to Cleared.
module Penny.Clearer where

import Penny.Copper

-- | Contains all command-line options.
data CommandLine = CommandLine
  { _ofxFile :: String
  -- ^ The OFX file the user wants to read.
  , _copperFiles :: Seq Filename
  -- ^ Copper files to read and, possibly, to edit.
  , _editFiles :: Bool
  -- ^ If True, edit the existing Copper files to make the changes.
  , _quiet :: Bool
  -- ^ If True, do not print the results to standard output.
