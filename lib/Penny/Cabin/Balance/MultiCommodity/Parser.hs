module Penny.Cabin.Balance.MultiCommodity.Parser (
  ParseOpts(..)
  , allSpecs
  ) where

import Control.Applicative ((<$))
import qualified Penny.Cabin.Colors as Col
import qualified Penny.Cabin.Options as CO
import qualified Penny.Cabin.Parsers as P
import qualified Penny.Lincoln as L
import qualified System.Console.MultiArg.Combinator as C

-- | Options for the Balance report that have been parsed from the
-- command line.
data ParseOpts = ParseOpts {
  drCrColors :: Col.DrCrColors
  , baseColors :: Col.BaseColors
  , colorPref :: CO.ColorPref
  , showZeroBalances :: CO.ShowZeroBalances
  , order :: L.SubAccount -> L.SubAccount -> Ordering
  }



color :: C.OptSpec (ParseOpts -> ParseOpts)
color = fmap toResult P.color
  where
    toResult c po = po { colorPref = c }


background :: C.OptSpec (ParseOpts -> ParseOpts)
background = fmap toResult P.background
  where
    toResult (dc, bc) po = po { drCrColors = dc
                              , baseColors = bc }


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
allSpecs =
  [ color
  , background ]
  ++ zeroBalances
  ++ [ ascending
     , descending
     ]
