module Penny.Cabin.Balance.Convert.Options where

import qualified Penny.Cabin.Balance.Convert.Parser as P
import qualified Penny.Cabin.Options as CO
import qualified Penny.Cabin.Colors as Col
import qualified Penny.Cabin.Colors.DarkBackground as Dark
import qualified Penny.Lincoln as L
import qualified Penny.Shield as S
import qualified Data.Text as X

-- | Default options for the Convert report. This record is used as
-- the starting point when parsing in options from the command
-- line. You don't need to use it if you are setting the options for
-- the Convert report directly from your own code.

data DefaultOpts = DefaultOpts {
    colorPref :: CO.ColorPref
  , drCrColors :: Col.DrCrColors
  , baseColors :: Col.BaseColors
  , showZeroBalances :: CO.ShowZeroBalances
  , target :: P.Target
  , dateTime :: L.DateTime
  , sortOrder :: P.SortOrder
  , sortBy :: P.SortBy
  , format :: L.Qty -> X.Text
  }

toParserOpts :: DefaultOpts -> P.Opts
toParserOpts d = P.Opts {
  P.colorPref = colorPref d
  , P.drCrColors = drCrColors d
  , P.baseColors = baseColors d
  , P.showZeroBalances = showZeroBalances d
  , P.target = target d
  , P.dateTime = dateTime d
  , P.sortOrder = sortOrder d
  , P.sortBy = sortBy d
  }

defaultOptions :: S.Runtime -> DefaultOpts
defaultOptions rt = DefaultOpts {
    colorPref = CO.PrefAuto
  , drCrColors = Dark.drCrColors
  , baseColors = Dark.baseColors
  , showZeroBalances = CO.ShowZeroBalances True
  , target = P.AutoTarget
  , dateTime = S.currentTime rt
  , sortOrder = P.Ascending
  , sortBy = P.SortByName 
  , format = X.pack . show . L.unQty
  }


