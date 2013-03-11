module Main where

import qualified Penny.Copper as C
import qualified Penny.Copper.Render as R
import qualified Data.Text.IO as TIO
import System.Environment (getArgs)

help :: String
help = unlines
  [ "usage: penny-reprint FILE..."
  , "Tidies the formatting of a Penny ledger file."
  , "All memos, comments, and blank lines are preserved,"
  , "and the order of the transactions and postings is not changed."
  , "However, the whitespace that separates different elements"
  , "of a posting will change in order to tidy things up."
  , ""
  , "If no FILE, or if FILE is \"-\", read stanard input."
  , "Result is printed to standard output."
  ]

groupSpecs :: R.GroupSpecs
groupSpecs = R.GroupSpecs R.NoGrouping R.NoGrouping

main :: IO ()
main = do
  as <- getArgs
  l <- C.open as
  case R.ledger groupSpecs l of
    Nothing -> error "could not render final ledger."
    Just x -> TIO.putStr x
