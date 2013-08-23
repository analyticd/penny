module Main where

import Data.Either (partitionEithers)
import qualified Penny.Copper as C
import qualified Penny.Copper.Render as R
import qualified Penny.Liberty as Ly
import qualified Data.Text as X
import qualified System.Console.MultiArg as MA

import qualified Paths_penny_bin as PPB

help :: String -> String
help pn = unlines
  [ "usage: " ++ pn ++ " FILE..."
  , "Tidies the formatting of a Penny ledger file."
  , "All memos, comments, and blank lines are preserved,"
  , "and the order of the transactions and postings is not changed."
  , "However, the whitespace that separates different elements"
  , "of a posting will change in order to tidy things up."
  , ""
  , "If no FILE, or if FILE is \"-\", read stanard input."
  , "Result is printed to standard output."
  , ""
  , "Options:"
  , "  --output FILENAME, -o FILENAME"
  , "    send output to FILENAME rather than standard output"
  , "    (multiple -o options are allowed; use \"-\" for standard"
  , "     output)"
  , "  --help, -h - show help and exit"
  , "  --version  - show version and exit"
  ]

type Printer = X.Text -> IO ()
type PosArg = String

type Arg = Either Printer PosArg

allOpts :: [MA.OptSpec Arg]
allOpts = [ fmap Left Ly.output ]

main :: IO ()
main = do
  as <- MA.simpleHelpVersion help (Ly.version PPB.version)
        allOpts MA.Intersperse
        (return . Right)
  let (printers, posArgs) = partitionEithers as
  l <- C.open posArgs
  case mapM (R.item Nothing) (map C.stripMeta l) of
    Nothing -> error "could not render final ledger."
    Just x ->
      let txt = X.concat x
      in txt `seq` (Ly.processOutput printers txt)
