module Penny.Cabin.Postings.Options where

import qualified Data.Text as X

import qualified Penny.Lincoln.Bits as B
import qualified Penny.Cabin.Postings.Colors as C

data Options =
  Options { drCrColors :: C.DrCrColors
          , baseColors :: C.BaseColors 
          , dateFormat :: B.DateTime -> X.Text }
