module Penny.Cabin.Balance.MultiCommodity.Parser (
  ParseOpts(..)
  , parseOptions
  ) where

import Control.Applicative (many, (<$))
import qualified Penny.Cabin.Colors as Col
import qualified Penny.Cabin.Options as CO
import qualified Penny.Cabin.Parsers as P
import qualified Penny.Lincoln as L
import qualified System.Console.MultiArg.Combinator as C
import System.Console.MultiArg.Prim (Parser)

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



-- | Parses all options for the Balance report from the command
-- line. Run this parser after parsing the name of the report
-- (e.g. @bal@ or @balance@) from the command line. This parser will
-- parse all words after the name of the report up to the the ledger
-- file names. The result is a computation, which can fail if the user
-- supplies a DateTime or a Commodity on the command line that fails
-- to parse or if the user supplies a argument for the @--background@
-- option that fails to parse.
parseOptions :: Parser (ParseOpts -> ParseOpts)
parseOptions = fmap toResult (many . C.parseOption $ allSpecs)
  where
    toResult fns o1 = foldl (flip (.)) id fns o1


allSpecs :: [C.OptSpec (ParseOpts -> ParseOpts)]
allSpecs =
  [ color
  , background ]
  ++ zeroBalances
  ++ [ ascending
     , descending
     ]
