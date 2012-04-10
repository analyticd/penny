module Main where

import qualified Data.List.NonEmpty as NE
import Data.List.NonEmpty (NonEmpty((:|)))
import qualified Penny.Brass as Br
import qualified Penny.Cabin.Interface as I
import qualified Penny.Cabin.Posts as P
import qualified Penny.Cabin.Balance as Bal
import qualified Penny.Copper as C
import qualified Penny.Lincoln.Bits as B
import qualified Penny.Zinc as Z

defaultTimeZone :: Br.DefaultTimeZone
defaultTimeZone = Br.utcDefault

radGroup :: Br.RadGroup
radGroup = Br.periodComma

cDefaultTimeZone :: C.DefaultTimeZone
cDefaultTimeZone = C.utcDefault

cRadGroup :: C.RadGroup
cRadGroup = C.periodComma

reports :: NE.NonEmpty I.Report
reports = (P.report f) :| [balRpt] where
  f rt = P.defaultOptions cDefaultTimeZone cRadGroup rt
  balRpt = Bal.balanceReport Bal.defaultOptions

main :: IO ()
main = Z.zincMain defaultTimeZone radGroup
       cDefaultTimeZone cRadGroup reports
