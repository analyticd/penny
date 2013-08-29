-- | Parsing options for the Convert report from the command line.
module Penny.Cabin.Balance.Convert.Parser (
  Opts(..)
  , Target(..)
  , SortBy(..)
  , RoundTo(..)
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


-- | Round to this many decimal places in the Percent report.
newtype RoundTo = RoundTo { unRoundTo :: L.NonNegative }
  deriving (Eq, Show, Ord)

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
  , percentRpt :: Maybe RoundTo
  -- ^ If the user wants a percentage report, set this.
  }

-- | Do not be tempted to change the setup in this module so that the
-- individual functions such as parseColor and parseBackground return
-- parsers rather than OptSpec. Such an arrangement breaks the correct
-- parsing of abbreviated long options.
allOptSpecs :: [C.OptSpec (Opts -> Opts)]
allOptSpecs =
  [ parseZeroBalances
  , parseCommodity
  , parseAuto
  , parseDate
  , parseSort
  , parseOrder
  , parsePct
  , parseRound
  ]

parseZeroBalances :: C.OptSpec (Opts -> Opts)
parseZeroBalances = fmap f P.zeroBalances
  where
    f x o = o { showZeroBalances = x }


parseCommodity :: C.OptSpec (Opts -> Opts)
parseCommodity = C.OptSpec ["commodity"] "c" (C.OneArgE f)
  where
    f a1 =
      case Parsec.parse Pc.lvl1Cmdty "" (X.pack a1) of
        Left _ -> Ex.throw . C.ErrorMsg $ "invalid commodity"
        Right g -> return $ \os -> os { target = ManualTarget . L.To $ g }

parseAuto :: C.OptSpec (Opts -> Opts)
parseAuto = C.OptSpec ["auto-commodity"] "" (C.NoArg f)
  where
    f os = os { target = AutoTarget }

parseDate :: C.OptSpec (Opts -> Opts)
parseDate = C.OptSpec ["date"] "d" (C.OneArgE f)
  where
    f a1 =
      case Parsec.parse Pc.dateTime "" (X.pack a1) of
        Left _ -> Ex.throw . C.ErrorMsg $ "invalid date"
        Right g -> return $ \os -> os { dateTime = g }

parseSort :: C.OptSpec (Opts -> Opts)
parseSort = C.OptSpec ["sort"] "s" (C.ChoiceArg ls)
  where
    ls = [ ("qty", (\os -> os { sortBy = SortByQty }))
         , ("name", (\os -> os { sortBy = SortByName })) ]

parseOrder :: C.OptSpec (Opts -> Opts)
parseOrder = fmap f P.order
  where
    f x o = o { sortOrder = x }

parsePct :: C.OptSpec (Opts -> Opts)
parsePct = C.OptSpec ["percent"] "%" (C.NoArg f)
  where
    f o = o { percentRpt = Just (RoundTo . maybe e id . L.nonNegative $ 0) }
    e = error $ "Penny.Cabin.Balance.Convert.Parser.parsePct: "
                ++ "error: zero is not non-negative"

parseRound :: C.OptSpec (Opts -> Opts)
parseRound = C.OptSpec ["round"] "r" (C.OneArgE f)
  where
    f a = do
      i <- C.reader a
      case L.nonNegative i of
        Nothing -> Ex.throw . C.ErrorMsg $ "argument is negative"
        Just g -> return $ \o -> o { percentRpt = Just (RoundTo g) }
