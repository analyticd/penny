module Penny.Cabin.Balance.Convert.Parser where

import Control.Applicative ((<$>), many)
import qualified Data.Text as X
import qualified Penny.Cabin.Options as CO
import qualified Penny.Cabin.Colors as Col
import qualified Penny.Cabin.Colors.DarkBackground as Dark
import qualified Penny.Cabin.Parsers as P
import qualified Penny.Lincoln as L
import qualified Penny.Liberty as Ly
import qualified Penny.Copper as Cop
import qualified Penny.Copper.Commodity as CC
import qualified Penny.Copper.DateTime as CD
import qualified System.Console.MultiArg.Combinator as C
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
  , drCrColors :: Col.DrCrColors
  , baseColors :: Col.BaseColors
  , showZeroBalances :: CO.ShowZeroBalances
  , target :: Target
  , dateTime :: L.DateTime
  , sortOrder :: SortOrder
  , sortBy :: SortBy
  }

defaultOptions :: L.DateTime -> Opts
defaultOptions dt = Opts {
  colorPref = CO.PrefAuto
  , drCrColors = Dark.drCrColors
  , baseColors = Dark.baseColors
  , showZeroBalances = CO.ShowZeroBalances True
  , target = AutoTarget
  , dateTime = dt
  , sortOrder = Ascending
  , sortBy = SortByName }

parseOpts :: Cop.DefaultTimeZone -> Parser (Opts -> Opts)
parseOpts dtz = fmap f (many (C.parseOption (allOptSpecs dtz)))
  where
    f = foldl (flip (.)) id

allOptSpecs :: Cop.DefaultTimeZone -> [C.OptSpec (Opts -> Opts)]
allOptSpecs dtz =
  [ parseColor
  , parseBackground ]
  ++ parseZeroBalances
  ++
  [ parseCommodity
  , parseDate dtz
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

parseDate ::
  Cop.DefaultTimeZone
  -> C.OptSpec (Opts -> Opts)
parseDate dtz = C.OptSpec ["date"] "d" (C.OneArg f)
  where
    f a1 os =
      case Parsec.parse (CD.dateTime dtz) "" (X.pack a1) of
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


