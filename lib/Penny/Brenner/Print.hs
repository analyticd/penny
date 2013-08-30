-- | Prints parsed transactions.
--
-- TODO add support to this and other Brenner components for reading
-- from stdin.
module Penny.Brenner.Print (mode) where

import qualified Penny.Brenner.Types as Y
import qualified Penny.Brenner.Util as U
import qualified System.Console.MultiArg as MA
import Data.Maybe (mapMaybe)

help :: String -> String
help pn = unlines
  [ "usage: " ++ pn ++ "  [global-options] print [local-options] FILE..."
  , "Parses the transactions in each FILE using the appropriate parser"
  , "and prints the parse result to standard output."
  , ""
  , "Local options:"
  , "  --help, -h Show this help and exit."
  ]

data Arg
  = ArgFile String

mode :: Y.Mode
mode mayFa = MA.modeHelp
  "print"
  help
  (processor mayFa)
  []
  MA.Intersperse
  (return . ArgFile)

processor
  :: Maybe Y.FitAcct
  -> [Arg]
  -> IO ()
processor mayFa ls = do
  fa <- U.getFitAcct mayFa
  doPrint (snd . Y.parser $ fa) ls

doPrint
  :: (Y.FitFileLocation -> IO (Either String [Y.Posting]))
  -> [Arg]
  -> IO ()
doPrint prsr ls = mapM_ f . mapMaybe toFile $ ls
  where
    f file = do
      r <- prsr file
      case r of
        Left s -> do
          fail $ "penny-fit print: error: " ++ s
        Right ps -> mapM putStr . map U.showPosting $ ps
    toFile a = case a of
      ArgFile s -> Just (Y.FitFileLocation s)

