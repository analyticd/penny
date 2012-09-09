module Penny.Cabin.Balance.MultiCommodity.Parser (
  Error(..)
  , ParseOpts(..)
  , parseOptions
  ) where

import Control.Applicative ((<|>), many, Applicative, pure,
                            (<$))
import Control.Monad ((>=>))
import qualified Control.Monad.Exception.Synchronous as Ex
import qualified Penny.Cabin.Colors as Col
import qualified Penny.Cabin.Options as CO
import qualified Penny.Cabin.Parsers as P
import qualified Penny.Lincoln as L
import System.Console.MultiArg.Prim (Parser)

-- | Options for the Balance report that have been parsed from the
-- command line.
data ParseOpts = ParseOpts {
  drCrColors :: Col.DrCrColors
  , baseColors :: Col.BaseColors
  , colorPref :: CO.ColorPref
  , showZeroBalances :: CO.ShowZeroBalances
  , order :: L.SubAccountName -> L.SubAccountName -> Ordering
  }


data Error = BadColorName String
           | BadBackground String
             deriving Show


color :: Parser (ParseOpts -> Ex.Exceptional Error ParseOpts)
color = fmap toResult P.color
  where
    toResult ex po =
      Ex.mapExceptional BadColorName (\c -> po { colorPref = c })
      ex


background :: Parser (ParseOpts -> Ex.Exceptional Error ParseOpts)
background = fmap toResult P.background
  where
    toResult ex po =
      Ex.mapExceptional BadBackground
      (\(dc, bc) -> po { drCrColors = dc
                       , baseColors = bc })
      ex

zeroBalances :: Parser (ParseOpts -> ParseOpts)
zeroBalances = fmap toResult P.zeroBalances
  where
    toResult szb o = o { showZeroBalances = szb }

ascending :: Parser (ParseOpts -> ParseOpts)
ascending = f <$ P.ascending 
  where
    f o = o { order = compare }

descending :: Parser (ParseOpts -> ParseOpts)
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
parseOptions :: Parser (ParseOpts
                        -> Ex.Exceptional Error ParseOpts)
parseOptions = fmap toResult (many parseOption)
  where
    toResult fns o1 = foldl (>=>) return fns o1


parseOption :: Parser (ParseOpts
                       -> Ex.Exceptional Error ParseOpts)
parseOption =
  color
  <|> background
  <|> impurify zeroBalances
  <|> impurify ascending
  <|> impurify descending

impurify ::
  (Applicative m, Functor f)
  => f (a -> a)
  -> f (a -> m a)
impurify = fmap (\f -> pure . f)
