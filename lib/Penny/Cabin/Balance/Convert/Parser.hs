-- | Parsing options for the Convert report from the command line.
module Penny.Cabin.Balance.Convert.Parser (
  Opts(..)
  , Target(..)
  , SortBy(..)
  , allOptSpecs
  ) where


import qualified Control.Monad.Exception.Synchronous as Ex
import qualified Data.Text as X
import qualified Penny.Cabin.Options as CO
import qualified Penny.Cabin.Parsers as P
import qualified Penny.Lincoln as L
import qualified Penny.Copper.Parsec as Pc
import qualified System.Console.MultiArg.Combinator as C
import qualified Text.Parsec as Parsec


-- | Is the target commodity determined by the user or automatically?
data Target = AutoTarget | ManualTarget L.To

data SortBy = SortByQty | SortByName deriving (Eq, Show, Ord)

-- | Default starting options for the Convert report. After
-- considering what is parsed in from the command line and price data,
-- a Convert.Opts will be generated.
data Opts = Opts
  { showZeroBalances :: CO.ShowZeroBalances
  , target :: Target
  , dateTime :: L.DateTime
  , sortOrder :: P.SortOrder
  , sortBy :: SortBy
  }

-- | Do not be tempted to change the setup in this module so that the
-- individual functions such as parseColor and parseBackground return
-- parsers rather than OptSpec. Such an arrangement breaks the correct
-- parsing of abbreviated long options.
allOptSpecs :: [C.OptSpec (Opts -> Ex.Exceptional String Opts)]
allOptSpecs =
  [ fmap toExc parseZeroBalances
  , parseCommodity
  , fmap toExc parseAuto
  , parseDate
  , fmap toExc parseSort
  , fmap toExc parseOrder ]
  where
    toExc f = return . f

parseZeroBalances :: C.OptSpec (Opts -> Opts)
parseZeroBalances = fmap f P.zeroBalances
  where
    f x o = o { showZeroBalances = x }


parseCommodity :: C.OptSpec (Opts -> Ex.Exceptional String Opts)
parseCommodity = C.OptSpec ["commodity"] "c" (C.OneArg f)
  where
    f a1 os =
      case Parsec.parse Pc.lvl1Cmdty "" (X.pack a1) of
        Left _ -> Ex.throw $ "invalid commodity: " ++ a1
        Right g -> return $ os { target = ManualTarget . L.To $ g }

parseAuto :: C.OptSpec (Opts -> Opts)
parseAuto = C.OptSpec ["auto-commodity"] "" (C.NoArg f)
  where
    f os = os { target = AutoTarget }

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

parseOrder :: C.OptSpec (Opts -> Opts)
parseOrder = fmap f P.order
  where
    f x o = o { sortOrder = x }
