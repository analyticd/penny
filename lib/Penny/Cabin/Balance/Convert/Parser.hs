module Penny.Cabin.Balance.Convert.Parser where

import Control.Applicative ((<$>), (<*>))
import qualified Penny.Cabin.Options as CO
import qualified Penny.Cabin.Colors as C
import qualified Penny.Cabin.Parsers as P
import qualified Penny.Lincoln as L
import qualified Penny.Copper as Cop
import qualified Penny.Copper.DateTime as CD
import qualified System.Console.MultiArg.Combinator as MC
import qualified Text.Parsec as Parsec
import System.Console.MultiArg.Prim (Parser)

-- | Is the target commodity determined by the user or automatically?
data Target = AutoTarget | ManualTarget L.To

type Sorter = (L.SubAccountName, L.BottomLine)
              -> (L.SubAccountName, L.BottomLine)
              -> Ordering

data SortOrder = Ascending | Descending
data SortBy = SortByQty | SortByName

-- | Default starting options for the Convert report. After
-- considering what is parsed in from the command line and price data,
-- a Convert.Opts will be generated.
data Opts = Opts {
  colorPref :: CO.ColorPref
  , drCrColors :: C.DrCrColors
  , baseColors :: C.BaseColors
  , showZeroBalances :: CO.ShowZeroBalances
  , target :: Target
  , dateTime :: L.DateTime
  , sortOrder :: SortOrder
  , sortBy :: SortBy
  }

parseColor :: Parser (Opts -> Opts)
parseColor = (\c o -> o { colorPref = c }) <$> P.color

parseBackground :: Parser (Opts -> Opts)
parseBackground = f <$> P.background
  where
    f (d, b) o = o { drCrColors = d, baseColors = b }

parseZeroBalances :: Parser (Opts -> Opts)
parseZeroBalances = (\z o -> o { showZeroBalances = z })
                    <$> P.zeroBalances

parseDate ::
  Cop.DefaultTimeZone
  -> Parser (Opts -> Opts)
parseDate dtz = P.parseOpt ["date"] "d" (MC.OneArg f)
