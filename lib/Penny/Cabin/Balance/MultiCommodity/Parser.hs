module Penny.Cabin.Balance.MultiCommodity.Parser (
  ParseOpts(..)
  , allSpecs
  ) where

import Control.Applicative ((<$))
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


zeroBalances :: [C.OptSpec (ParseOpts -> ParseOpts)]
zeroBalances = map (fmap toResult) P.zeroBalances
  where
    toResult szb o = o { showZeroBalances = szb }

ascending :: C.OptSpec (ParseOpts -> ParseOpts)
ascending = f <$ P.ascending
  where
    f o = o { order = compare }

descending :: C.OptSpec (ParseOpts -> ParseOpts)
descending = f <$ P.descending
  where
    f o = o { order = CO.descending compare }


allSpecs :: [C.OptSpec (ParseOpts -> ParseOpts)]
allSpecs = zeroBalances ++ [ ascending , descending ]
