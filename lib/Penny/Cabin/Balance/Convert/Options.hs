-- | Default options for the Convert report when used from the command
-- line.
module Penny.Cabin.Balance.Convert.Options where

import qualified Penny.Cabin.Balance.Convert.Parser as P
import qualified Penny.Cabin.Options as CO
import qualified Penny.Lincoln as L
import qualified Penny.Shield as S
import qualified Data.Text as X

-- | Default options for the Convert report. This record is used as
-- the starting point when parsing in options from the command
-- line. You don't need to use it if you are setting the options for
-- the Convert report directly from your own code.

data DefaultOpts = DefaultOpts
  { showZeroBalances :: CO.ShowZeroBalances
  , target :: P.Target
  , dateTime :: L.DateTime
  , sortOrder :: P.SortOrder
  , sortBy :: P.SortBy
  , format :: L.Qty -> X.Text
  }

toParserOpts :: DefaultOpts -> P.Opts
toParserOpts d = P.Opts
  { P.showZeroBalances = showZeroBalances d
  , P.target = target d
  , P.dateTime = dateTime d
  , P.sortOrder = sortOrder d
  , P.sortBy = sortBy d
  , P.showHelp = False
  }

defaultOptions :: S.Runtime -> DefaultOpts
defaultOptions rt = DefaultOpts
  { showZeroBalances = CO.ShowZeroBalances False
  , target = P.AutoTarget
  , dateTime = S.currentTime rt
  , sortOrder = P.Ascending
  , sortBy = P.SortByName
  , format = X.pack . show
  }


