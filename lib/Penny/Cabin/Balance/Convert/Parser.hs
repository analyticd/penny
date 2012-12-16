-- | Parsing options for the Convert report from the command line.
module Penny.Cabin.Balance.Convert.Parser (
  Opts(..)
  , Target(..)
  , Sorter
  , SortOrder(..)
  , SortBy(..)
  , allOptSpecs
  ) where


import Control.Applicative ((<$>))
import qualified Control.Monad.Exception.Synchronous as Ex
import qualified Data.Text as X
import qualified Penny.Cabin.Options as CO
import qualified Penny.Cabin.Colors as Col
import qualified Penny.Cabin.Parsers as P
import qualified Penny.Lincoln as L
import qualified Penny.Copper.Parsec as Pc
import qualified System.Console.MultiArg.Combinator as C
import qualified Text.Parsec as Parsec


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

-- | Do not be tempted to change the setup in this module so that the
-- individual functions such as parseColor and parseBackground return
-- parsers rather than OptSpec. Such an arrangement breaks the correct
-- parsing of abbreviated long options.
allOptSpecs :: [C.OptSpec (Opts -> Ex.Exceptional String Opts)]
allOptSpecs =
  [ fmap toExc parseColor
  , fmap toExc parseBackground ]
  ++ map (fmap toExc) parseZeroBalances
  ++
  [ parseCommodity
  , parseDate
  , fmap toExc parseSort
  , fmap toExc parseAscending
  , fmap toExc parseDescending ]
  where
    toExc f = return . f

parseColor ::  C.OptSpec (Opts -> Opts)
parseColor = (\c o -> o { colorPref = c }) <$> P.color

parseBackground :: C.OptSpec (Opts -> Opts)
parseBackground = fmap f P.background
  where
    f (d, b) o = o { drCrColors = d, baseColors = b }

parseZeroBalances :: [C.OptSpec (Opts -> Opts)]
parseZeroBalances =
  map (fmap (\z o -> o { showZeroBalances = z })) P.zeroBalances


parseCommodity :: C.OptSpec (Opts -> Ex.Exceptional String Opts)
parseCommodity = C.OptSpec ["commodity"] "c" (C.OneArg f)
  where
    f a1 os =
      case Parsec.parse Pc.lvl1Cmdty "" (X.pack a1) of
        Left _ -> Ex.throw $ "invalid commodity: " ++ a1
        Right g -> return $ os { target = ManualTarget . L.To $ g }

parseDate :: C.OptSpec (Opts -> Ex.Exceptional String Opts)
parseDate = C.OptSpec ["date"] "d" (C.OneArg f)
  where
    f a1 os =
      case Parsec.parse Pc.dateTime "" (X.pack a1) of
        Left _ -> Ex.throw $ "invalid date: " ++ a1
        Right g -> return $ os { dateTime = g }

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


