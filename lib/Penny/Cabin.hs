-- | Cabin - Penny reports
module Penny.Cabin (allReportsWithDefaults) where

import Data.List.NonEmpty (NonEmpty((:|)))
import qualified Data.List.NonEmpty as NE

import qualified Penny.Cabin.Balance as B
import qualified Penny.Cabin.Posts as P
import qualified Penny.Copper as C
import qualified Penny.Cabin.Interface as I

allReportsWithDefaults ::
  C.DefaultTimeZone
  -> C.RadGroup
  -> NE.NonEmpty I.Report
allReportsWithDefaults dtz rg =
  B.defaultBalanceReport :| [P.defaultPostsReport dtz rg]
  
