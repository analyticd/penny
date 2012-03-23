module Main where

import Data.Text ( pack )
import qualified Data.Text.IO as TIO
import System.Environment
import Text.Parsec
import qualified Text.PrettyPrint as PP

import qualified Penny.Copper as C
import Penny.Lincoln
import PennyTest.Pretty

main :: IO ()
main = do
  let dtz = C.DefaultTimeZone noOffset
  (a:[]) <- getArgs
  f <- TIO.readFile a
  let rg = C.periodComma
      fn = Filename (pack a)
      e = parse (C.ledger fn dtz rg) a f
  putStrLn (PP.render . pretty $ e)

