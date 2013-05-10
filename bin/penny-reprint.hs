module Main where

import qualified Penny.Copper as C
import qualified Penny.Copper.Render as R
import qualified Penny.Liberty as Ly
import qualified Data.Text.IO as TIO
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
  , "  --help, -h - show help and exit"
  , "  --version  - show version and exit"
  ]

groupSpecs :: R.GroupSpecs
groupSpecs = R.GroupSpecs R.NoGrouping R.NoGrouping

type MaybeShowVer = IO ()
type Opts = (MaybeShowVer, [String])

posArg :: String -> Opts -> Opts
posArg s (a, ss) = (a, s:ss)

allOpts :: [MA.OptSpec (Opts -> Opts)]
allOpts = [ fmap (\a (_, ss) -> (a, ss))
            $ Ly.version PPB.version ]

main :: IO ()
main = do
  as <- MA.simpleWithHelp help MA.Intersperse allOpts posArg
  let opts = foldr ($) (return (), []) as
  fst opts
  l <- C.open . snd $ opts
  case mapM (R.item groupSpecs) l of
    Nothing -> error "could not render final ledger."
    Just x -> mapM_ TIO.putStr x
