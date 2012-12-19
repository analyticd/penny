-- | Prints parsed transactions.
--
-- TODO add support to this and other Brenner components for reading
-- from stdin.
module Penny.Brenner.Print (mode) where

import qualified Penny.Brenner.Types as Y
import qualified Penny.Brenner.Util as U
import qualified System.Console.MultiArg as MA
import qualified System.IO as IO
import qualified System.Exit as E
import qualified Control.Monad.Exception.Synchronous as Ex
import Data.Maybe (mapMaybe)

help :: String
help = unlines
  [ "penny-fit [global-options] print [local-options] FILE..."
  , "Parses the transactions in each FILE using the appropriate parser"
  , "and prints the parse result to standard output."
  , ""
  , "Local options:"
  , "  --help, -h Show this help and exit."
  ]

data Arg
  = ArgHelp
  | ArgFile String
  deriving (Eq, Show)

mode
  :: (Y.FitFileLocation -> IO (Ex.Exceptional String [Y.Posting]))
  -> MA.Mode String (Ex.Exceptional String (IO ()))
mode prsr = MA.Mode
  { MA.mId = "print"
  , MA.mName = "print"
  , MA.mIntersperse = MA.Intersperse
  , MA.mOpts = [MA.OptSpec ["help"] "h" (MA.NoArg ArgHelp)]
  , MA.mPosArgs = ArgFile
  , MA.mProcess = processor prsr
  }

processor
  :: (Y.FitFileLocation -> IO (Ex.Exceptional String [Y.Posting]))
  -> [Arg]
  -> Ex.Exceptional String (IO ())
processor prsr ls =
  if any (== ArgHelp) ls
  then return (putStrLn help)
  else return (mapM_ f . mapMaybe toFile $ ls)
  where
    f file = do
      r <- prsr file
      case r of
        Ex.Exception s -> do
          IO.hPutStrLn IO.stderr $ "penny-fit import: error: " ++ s
          E.exitFailure
        Ex.Success ps -> mapM putStr . map U.showPosting $ ps
    toFile a = case a of
      ArgFile s -> Just (Y.FitFileLocation s)
      _ -> Nothing

