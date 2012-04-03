-- | Options for the Balance report.
module Penny.Cabin.Balance.Options where

import qualified Penny.Lincoln as L
import qualified Penny.Cabin.Chunk as Chunk
import qualified Penny.Cabin.Colors as C

data Options = Options {
  drCrColors :: C.DrCrColors
  , baseColors :: C.BaseColors
  , balanceFormat :: L.Account -> L.Balance -> X.Text
  , colorPref :: Chunk.ColorPref
  } deriving Show
