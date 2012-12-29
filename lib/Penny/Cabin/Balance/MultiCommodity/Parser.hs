module Penny.Cabin.Balance.MultiCommodity.Parser (
  ParseOpts(..)
  , allSpecs
  ) where

import qualified Penny.Cabin.Options as CO
import qualified Penny.Cabin.Parsers as P
import qualified Penny.Lincoln as L
import qualified System.Console.MultiArg.Combinator as C

-- | Options for the Balance report that have been parsed from the
-- command line.
data ParseOpts = ParseOpts
  { showZeroBalances :: CO.ShowZeroBalances
  , order :: L.SubAccount -> L.SubAccount -> Ordering
  }


zeroBalances :: C.OptSpec (ParseOpts -> ParseOpts)
zeroBalances = fmap toResult P.zeroBalances
  where
    toResult szb o = o { showZeroBalances = szb }

parseOrder :: C.OptSpec (ParseOpts -> ParseOpts)
parseOrder = fmap toResult P.order
  where
    toResult x o = o { order = r }
      where
        r = case x of
          P.Ascending -> compare
          P.Descending -> CO.descending compare

allSpecs :: [C.OptSpec (ParseOpts -> ParseOpts)]
allSpecs = [zeroBalances, parseOrder]
