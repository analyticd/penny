module Main where

import Data.Text ( pack )
import qualified Data.Text.IO as TIO
import Data.Time
import System.Environment
import Text.Parsec
import Text.PrettyPrint

import Penny.Copper
import PennyTest.Pretty

main :: IO ()
main = do
  dtz <- fmap DefaultTimeZone getCurrentTimeZone
  (a:[]) <- getArgs
  f <- TIO.readFile a
  let rg = periodComma
      fn = Filename (pack a)
      e = parse (ledger fn dtz rg) a f
  putStrLn (render . pretty $ e)

