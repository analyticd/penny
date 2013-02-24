module Penny.Cabin.Balance.MultiCommodity.Parser (
  ParseOpts(..)
  , allSpecs
  ) where

import qualified Penny.Cabin.Options as CO
import qualified Penny.Cabin.Parsers as P
import qualified System.Console.MultiArg as MA

-- | Options for the Balance report that have been parsed from the
-- command line.
data ParseOpts = ParseOpts
  { showZeroBalances :: CO.ShowZeroBalances
  , order :: P.SortOrder
  }


zeroBalances :: MA.OptSpec (ParseOpts -> ParseOpts)
zeroBalances = fmap toResult P.zeroBalances
  where
    toResult szb o = o { showZeroBalances = szb }

parseOrder :: MA.OptSpec (ParseOpts -> ParseOpts)
parseOrder = fmap toResult P.order
  where
    toResult x o = o { order = x }

allSpecs :: [MA.OptSpec (ParseOpts -> ParseOpts)]
allSpecs = [zeroBalances, parseOrder]
