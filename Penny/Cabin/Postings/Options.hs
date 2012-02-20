module Penny.Cabin.Postings.Options where

import qualified Data.Text as X

import qualified Penny.Lincoln.Bits as B
import qualified Penny.Cabin.Postings.Colors as C
import qualified Penny.Cabin.Postings.Types as T

data Options =
  Options { drCrColors :: C.DrCrColors
          , baseColors :: C.BaseColors 
          , dateFormat :: T.PostingInfo -> X.Text
          , qtyFormat :: T.PostingInfo -> X.Text }
