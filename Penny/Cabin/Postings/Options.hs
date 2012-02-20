module Penny.Cabin.Postings.Options where

import qualified Data.Text as X

import qualified Penny.Lincoln.Balance as Bal
import qualified Penny.Lincoln.Bits as Bits
import qualified Penny.Cabin.Allocate as A
import qualified Penny.Cabin.Postings.Colors as C
import qualified Penny.Cabin.Postings.Types as T

data Options =
  Options { drCrColors :: C.DrCrColors
          , baseColors :: C.BaseColors 
          , dateFormat :: T.PostingInfo -> X.Text
          , qtyFormat :: T.PostingInfo -> X.Text
          , balanceFormat :: Bits.Commodity -> Bal.Nought -> X.Text }

