module Main where

import qualified Data.List.NonEmpty as NE
import Data.Time (utc)
import qualified Penny.Cabin.Interface as I
import qualified Penny.Cabin.Postings as P
import qualified Penny.Copper as C
import qualified Penny.Zinc as Z

defaultTimeZone = C.DefaultTimeZone utc
radSep = C.radixAndSeparator '.' ','
radix = fst radSep
separator = snd radSep

postings :: NE.NonEmpty I.Report
postings = NE.nonEmpty (P.report f) [] where
  f rt = (P.defaultFields,
          P.defaultOptions defaultTimeZone radix separator rt)

main :: IO ()
main = Z.zincMain defaultTimeZone radix separator postings
