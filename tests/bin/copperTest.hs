module Main where

import Data.Text ( pack )
import qualified Data.Text.IO as TIO
import System.Environment
import Text.Parsec
import Text.PrettyPrint

import Penny.Copper
import Penny.Lincoln.Bits
import PennyTest.Pretty

main :: IO ()
main = do
  let dtz = DefaultTimeZone (TimeZoneOffset 0)
  (a:[]) <- getArgs
  f <- TIO.readFile a
  let rg = periodComma
      fn = Filename (pack a)
      e = parse (ledger fn dtz rg) a f
  putStrLn (render . pretty $ e)

