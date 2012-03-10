module Main where

import qualified Data.List.NonEmpty as NE
import Data.List.NonEmpty (NonEmpty((:|)))
import Data.Time (utc)
import qualified Penny.Cabin.Interface as I
import qualified Penny.Cabin.Postings as P
import qualified Penny.Copper as C
import qualified Penny.Zinc as Z

defaultTimeZone :: C.DefaultTimeZone
defaultTimeZone = C.DefaultTimeZone utc

radGroup :: C.RadGroup
radGroup = C.periodComma

postings :: NE.NonEmpty I.Report
postings = (P.report f) :| [] where
  f rt = P.defaultOptions defaultTimeZone radGroup rt

main :: IO ()
main = Z.zincMain defaultTimeZone radGroup postings
