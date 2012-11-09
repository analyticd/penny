-- | Parsing options for the Convert report from the command line.
module Penny.Cabin.Balance.Convert.Parser (
  Opts(..)
  , Target(..)
  , Sorter
  , SortOrder(..)
  , SortBy(..)
  , parseOpts)
  where

import Control.Applicative ((<$>), many)
import qualified Data.Text as X
import qualified Penny.Cabin.Options as CO
import qualified Penny.Cabin.Colors as Col
import qualified Penny.Cabin.Parsers as P
import qualified Penny.Lincoln as L
import qualified Penny.Liberty as Ly
import qualified Penny.Copper.Commodity as CC
import qualified Penny.Copper.DateTime as CD
import qualified System.Console.MultiArg.Combinator as C
import qualified Text.Parsec as Parsec
import System.Console.MultiArg.Prim (Parser)

-- | Is the target commodity determined by the user or automatically?
data Target = AutoTarget | ManualTarget L.To

type Sorter = (L.SubAccount, L.BottomLine)
              -> (L.SubAccount, L.BottomLine)
              -> Ordering

data SortOrder = Ascending | Descending
data SortBy = SortByQty | SortByName

-- | Default starting options for the Convert report. After
-- considering what is parsed in from the command line and price data,
-- a Convert.Opts will be generated.
data Opts = Opts {
  colorPref :: CO.ColorPref
  , drCrColors :: Col.DrCrColors
  , baseColors :: Col.BaseColors
  , showZeroBalances :: CO.ShowZeroBalances
  , target :: Target
  , dateTime :: L.DateTime
  , sortOrder :: SortOrder
  , sortBy :: SortBy
  }

-- | Parses all options for the Convert report.
parseOpts :: Parser (Opts -> Opts)
parseOpts = fmap f (many (C.parseOption allOptSpecs))
  where
    f = foldl (flip (.)) id

-- | Do not be tempted to change the setup in this module so that the
-- individual functions such as parseColor and parseBackground return
-- parsers rather than OptSpec. Such an arrangement breaks the correct
-- parsing of abbreviated long options.
allOptSpecs :: [C.OptSpec (Opts -> Opts)]
allOptSpecs =
  [ parseColor
  , parseBackground ]
  ++ parseZeroBalances
  ++
  [ parseCommodity
  , parseDate
  , parseSort
  , parseAscending
  , parseDescending ]

parseColor ::  C.OptSpec (Opts -> Opts)
parseColor = (\c o -> o { colorPref = c }) <$> P.color

parseBackground :: C.OptSpec (Opts -> Opts)
parseBackground = fmap f P.background
  where
    f (d, b) o = o { drCrColors = d, baseColors = b }

parseZeroBalances :: [C.OptSpec (Opts -> Opts)]
parseZeroBalances =
  map (fmap (\z o -> o { showZeroBalances = z })) P.zeroBalances


parseCommodity :: C.OptSpec (Opts -> Opts)
parseCommodity = C.OptSpec ["commodity"] "c" (C.OneArg f)
  where
    f a1 os =
      case Parsec.parse CC.lvl1Cmdty "" (X.pack a1) of
        Left _ -> Ly.abort $ "invalid commodity: " ++ a1
        Right g -> os { target = ManualTarget . L.To $ g }

parseDate :: C.OptSpec (Opts -> Opts)
parseDate = C.OptSpec ["date"] "d" (C.OneArg f)
  where
    f a1 os =
      case Parsec.parse CD.dateTime "" (X.pack a1) of
        Left _ -> Ly.abort $ "invalid date: " ++ a1
        Right g -> os { dateTime = g }

parseSort :: C.OptSpec (Opts -> Opts)
parseSort = C.OptSpec ["sort"] "s" (C.ChoiceArg ls)
  where
    ls = [ ("qty", (\os -> os { sortBy = SortByQty }))
         , ("name", (\os -> os { sortBy = SortByName })) ]

parseAscending :: C.OptSpec (Opts -> Opts)
parseAscending = C.OptSpec ["ascending"] "" (C.NoArg f)
  where
    f os = os { sortOrder = Ascending }

parseDescending :: C.OptSpec (Opts -> Opts)
parseDescending = C.OptSpec ["descending"] "" (C.NoArg f)
  where
    f os = os { sortOrder = Descending }


