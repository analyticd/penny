module Penny.Cabin.Balance.MultiCommodity.Parser (
  Error(..)
  , ParseOpts(..)
  , parseOptions
  ) where

import Control.Applicative ((<|>), many, Applicative, pure)
import Control.Monad ((>=>))
import qualified Control.Monad.Exception.Synchronous as Ex
import qualified Penny.Cabin.Colors as Col
import qualified Penny.Cabin.Chunk as Chk
import qualified Penny.Cabin.Options as CO
import qualified Penny.Cabin.Parsers as P
import qualified Penny.Copper.DateTime as CD
import qualified Penny.Shield as S
import System.Console.MultiArg.Prim (Parser)

-- | Options for the Balance report that have been parsed from the command line.
data ParseOpts = ParseOpts {
  drCrColors :: Col.DrCrColors
  , baseColors :: Col.BaseColors
  , colorPref :: Chk.Colors
  , showZeroBalances :: CO.ShowZeroBalances
  }


data Error = BadColorName String
           | BadBackground String
             deriving Show


color :: Parser (S.Runtime
                 -> ParseOpts
                 -> Ex.Exceptional Error ParseOpts)
color = fmap toResult P.color
  where
    toResult toEx rt po =
      Ex.mapExceptional BadColorName (\c -> po { colorPref = c })
      (toEx rt)


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

-- | Parses all options for the Balance report from the command
-- line. Run this parser after parsing the name of the report
-- (e.g. @bal@ or @balance@) from the command line. This parser will
-- parse all words after the name of the report up to the the ledger
-- file names. The result is a computation, which can fail if the user
-- supplies a DateTime or a Commodity on the command line that fails
-- to parse or if the user supplies a argument for the @--background@
-- option that fails to parse.
parseOptions :: Parser (S.Runtime
                        -> CD.DefaultTimeZone
                        -> ParseOpts
                        -> Ex.Exceptional Error ParseOpts)
parseOptions = fmap toResult (many parseOption)
  where
    toResult fns rt dtz o1 =
      let fns' = map (\fn -> fn rt dtz) fns
      in foldl (>=>) return fns' o1

parseOption :: Parser (S.Runtime
                       -> CD.DefaultTimeZone
                       -> ParseOpts
                       -> Ex.Exceptional Error ParseOpts)
parseOption =
  fmap (\toEx rt _ o -> toEx rt o) color
  <|> wrap background
  <|> wrap (impurify zeroBalances)
  where
    wrap = fmap (\toEx _ _ op -> toEx op)

impurify ::
  (Applicative m, Functor f)
  => f (a -> a)
  -> f (a -> m a)
impurify = fmap (\f -> pure . f)
