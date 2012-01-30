module Main where

import Control.Monad
import Data.Time
import Text.Parsec
import qualified Data.Text.IO as TIO
import System.Environment
import Text.Show.Pretty

import Penny.Parser
import Penny.Parser.DateTime
import Penny.Parser.Qty

main :: IO ()
main = do
  dtz <- liftM DefaultTimeZone getCurrentTimeZone
  (a:[]) <- getArgs
  f <- TIO.readFile a
  let (rad, sep) = radixAndSeparator '.' ','
      e = parse (ledger dtz rad sep) a f
  putStrLn $ ppShow e

