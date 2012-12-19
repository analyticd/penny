-- | Prints parsed transactions.
--
-- TODO add support to this and other Brenner components for reading
-- from stdin.
module Penny.Brenner.Print where

import qualified Penny.Brenner.Types as Y
import qualified System.Console.MultiArg as MA

help :: String
help = unlines
  [ "penny-fit [global-options] print FILE..."
  , "Parses the transactions in each FILE using the appropriate parser"
  , "and prints the parse result to standard output."
  ]

mode
  :: (Y.FitFileLocation -> IO (Ex.Exceptional String [Y.Posting]))
  -> MA.Mode String (Ex.Exceptional String (IO ()))
mode prsr = MA.Mode
  { MA.mId = "print"
  , MA.mName = "print"
  , MA.mIntersperse = MA.Intersperse
  , MA.mOpts = []
  , MA.mPosArgs = Y.FitFileLocation
  , MA.mProcess = processor prsr
  }

processor
  :: (Y.FitFileLocation -> IO (Ex.Exceptional String [Y.Posting]))
  -> [Y.FitFileLocation]
  -> Ex.Exceptional 
