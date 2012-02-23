module Penny.Cabin.Postings.Options where

import qualified Data.Text as X
import Data.Time (formatTime)
import System.Locale (defaultTimeLocale)
import qualified Data.Time as Time

import qualified Penny.Lincoln.Balance as Bal
import qualified Penny.Lincoln.Bits as Bits
import qualified Penny.Lincoln.Queries as Q
import qualified Penny.Cabin.Allocate as A
import qualified Penny.Cabin.Postings.Colors as C
import qualified Penny.Cabin.Postings.Types as T

data Options =
  Options { drCrColors :: C.DrCrColors
          , baseColors :: C.BaseColors 
          , dateFormat :: T.PostingInfo -> X.Text
          , qtyFormat :: T.PostingInfo -> X.Text
          , balanceFormat :: Bits.Commodity -> Bal.Nought -> X.Text
          , payeeAllocation :: A.Allocation
          , accountAllocation :: A.Allocation 
          , width :: ReportWidth
          , subAccountLength :: Int }

newtype ReportWidth = ReportWidth { unReportWidth :: Int }
                      deriving (Eq, Show, Ord)

defaultDateFormat :: T.PostingInfo -> X.Text
defaultDateFormat p = X.pack (formatTime defaultTimeLocale fmt d) where
  d = Time.utctDay . Bits.unDateTime . Q.dateTime . T.postingBox $ p
  fmt = "%Y-%m-%d"


