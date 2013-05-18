module Main where

import qualified System.Console.MultiArg as MA
import qualified Control.Monad.Exception.Synchronous as Ex
import qualified Test.QuickCheck.Gen as G
import qualified System.Random as Rand
import Control.Monad (replicateM)
import qualified Copper.Gen.Parsers as P
import qualified Penny.Copper.Render as R
import qualified System.Exit as Exit
import qualified Data.Text.IO as TIO
import qualified System.IO as IO

help :: String -> String
help pn = unlines
  [ "usage: " ++ pn ++ " [options]"
  , "Print a nonsensical but valid Penny file to standard output."
  , "Options:"
  , "  -s, --size INT"
  , "      QuickCheck size parameter. Bigger numbers give more"
  , "      gibberish. (default: 100)"
  , "  -c, --count INT"
  , "      Number of items (transactions, comments, prices, and"
  , "      blank lines, total) to output. (default: 100)"
  , "  -l, --left GROUP_SPEC"
  , "      Group left of the decimal point (default: none)"
  , "  -r, --right GROUP_SPEC"
  , "      Group right of the decimal point (default: none)"
  , "      where GROUP_SPEC is:"
  , "        none - no digit grouping"
  , "        large - group if greater than 9,999 left of decimal,"
  , "                or more than 4 decimal places right of decimal"
  , "        all - group whenever there are at least 4 places"
  ]

data Opts = Opts
  { optSize :: Int
  , optCount :: Int
  , optLeft :: R.GroupSpec
  , optRight :: R.GroupSpec
  } deriving Show

defaultOpts :: Opts
defaultOpts = Opts 100 100 R.NoGrouping R.NoGrouping

options :: [MA.OptSpec (Opts -> Opts)]
options =
  [ MA.OptSpec ["size"] "s" . MA.OneArgE $ \s -> do
      i <- MA.reader s
      if i < 1
        then Ex.throw (MA.ErrorMsg "non-positive size parameter")
        else return (\os -> os { optSize = i })

  , MA.OptSpec ["count"] "c" . MA.OneArgE $ \s -> do
      i <- MA.reader s
      if i < 1
        then Ex.throw (MA.ErrorMsg "non-positive count parameter")
        else return (\os -> os { optCount = i })

  , MA.OptSpec ["left"] "l" . MA.ChoiceArg
    . map (\(str, spec) -> (str, \os -> os { optLeft = spec }))
    $ groupSpecs

  , MA.OptSpec ["right"] "r" . MA.ChoiceArg
    . map (\(str, spec) -> (str, \os -> os { optRight = spec }))
    $ groupSpecs
  ]

groupSpecs :: [(String, R.GroupSpec)]
groupSpecs = [ ("none",  R.NoGrouping)
             , ("large", R.GroupLarge)
             , ("all",   R.GroupAll  ) ]

posArg :: a -> Ex.Exceptional MA.InputError b
posArg _ = Ex.throw (MA.ErrorMsg "no non-option arguments accepted")

parse :: [(Opts -> Opts)] -> Opts
parse os = foldl (flip (.)) id os defaultOpts

main :: IO ()
main = do
  pn <- MA.getProgName
  os <- fmap parse $ MA.simpleWithHelp help MA.Intersperse options
                     posArg
  gen <- Rand.getStdGen
  let is = (\g -> G.unGen g gen (optSize os))
           . fmap (map fst)
           . replicateM (optCount os)
           $ P.item
      gs = R.GroupSpecs (optLeft os) (optRight os)
      x = mapM (R.item gs) is
  case x of
    Nothing -> do
      IO.hPutStrLn IO.stderr $ pn ++ ": error: could not render ledger."
      Exit.exitFailure
    Just strs -> mapM_ TIO.putStr strs >> Exit.exitSuccess
