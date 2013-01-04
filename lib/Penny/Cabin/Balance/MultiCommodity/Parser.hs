module Penny.Cabin.Balance.MultiCommodity.Parser (
  ParseOpts(..)
  , allSpecs
  ) where

import qualified Penny.Cabin.Options as CO
import qualified Penny.Cabin.Parsers as P
import qualified Penny.Lincoln as L
import qualified System.Console.MultiArg as MA

-- | Options for the Balance report that have been parsed from the
-- command line.
data ParseOpts = ParseOpts
  { showZeroBalances :: CO.ShowZeroBalances
  , order :: L.SubAccount -> L.SubAccount -> Ordering
  , needsHelp :: Bool
  }


parseHelp :: MA.OptSpec (ParseOpts -> ParseOpts)
parseHelp = fmap f P.help
  where
    f _ o = o { needsHelp = True }

zeroBalances :: MA.OptSpec (ParseOpts -> ParseOpts)
zeroBalances = fmap toResult P.zeroBalances
  where
    toResult szb o = o { showZeroBalances = szb }

parseOrder :: MA.OptSpec (ParseOpts -> ParseOpts)
parseOrder = fmap toResult P.order
  where
    toResult x o = o { order = r }
      where
        r = case x of
          P.Ascending -> compare
          P.Descending -> CO.descending compare

allSpecs :: [MA.OptSpec (ParseOpts -> ParseOpts)]
allSpecs = [parseHelp, zeroBalances, parseOrder]
