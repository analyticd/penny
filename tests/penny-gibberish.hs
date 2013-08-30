module Main where

import qualified System.Console.MultiArg as MA
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
  , "Uses modified generators that only make printable ASCII."
  , "Options:"
  , "  -s, --size INT"
  , "      QuickCheck size parameter. Bigger numbers give more"
  , "      gibberish. (default: 5)"
  , "  -c, --count INT"
  , "      Number of items (transactions, comments, prices, and"
  , "      blank lines, total) to output. (default: 100)"
  ]

data Opts = Opts
  { optSize :: Int
  , optCount :: Int
  } deriving Show

defaultOpts :: Opts
defaultOpts = Opts 5 100

options :: [MA.OptSpec (Opts -> Opts)]
options =
  [ MA.OptSpec ["size"] "s" . MA.OneArgE $ \s -> do
      i <- MA.reader s
      if i < 1
        then Left (MA.ErrorMsg "non-positive size parameter")
        else return (\os -> os { optSize = i })

  , MA.OptSpec ["count"] "c" . MA.OneArgE $ \s -> do
      i <- MA.reader s
      if i < 1
        then Left (MA.ErrorMsg "non-positive count parameter")
        else return (\os -> os { optCount = i })
  ]

posArg :: a -> Either MA.InputError b
posArg _ = Left (MA.ErrorMsg "no non-option arguments accepted")

parse :: [(Opts -> Opts)] -> Opts
parse os = foldl (flip (.)) id os defaultOpts

main :: IO ()
main = do
  pn <- MA.getProgName
  os <- fmap parse $ MA.simpleHelp help options MA.Intersperse
                     posArg
  gen <- Rand.getStdGen
  let is = (\g -> G.unGen g gen (optSize os))
           . fmap (map fst)
           . replicateM (optCount os)
           $ P.item
      x = mapM (R.item Nothing) is
  case x of
    Nothing -> do
      IO.hPutStrLn IO.stderr $ pn ++ ": error: could not render ledger."
      IO.hPutStrLn IO.stderr $ pn ++ "bad ledger: " ++ show is
      Exit.exitFailure
    Just strs -> mapM_ TIO.putStr strs >> Exit.exitSuccess
