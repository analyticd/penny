-- | Command line parsers that are common to various Cabin reports.

module Penny.Cabin.Parsers where

import Control.Applicative ((<|>))
import qualified Control.Monad.Exception.Synchronous as Ex
import qualified Penny.Cabin.Chunk as Chk
import qualified Penny.Cabin.Colors as Col
import qualified Penny.Cabin.Colors.DarkBackground as DB
import qualified Penny.Cabin.Colors.LightBackground as LB
import qualified Penny.Cabin.Options as CO
import qualified Penny.Shield as S
import qualified System.Console.MultiArg.Combinator as C
import System.Console.MultiArg.Prim (Parser)

parseOpt :: [String] -> [Char] -> C.ArgSpec a -> Parser a
parseOpt ss cs a = C.parseOption [C.OptSpec ss cs a]

processColorArg ::
  S.Runtime
  -> String
  -> Maybe Chk.Colors
processColorArg rt x
  | x == "yes" = return Chk.Colors8
  | x == "no" = return Chk.Colors0
  | x == "auto" = return (CO.maxCapableColors rt)
  | x == "256" = return Chk.Colors256
  | otherwise = Nothing

-- | Process an argument for how many colors the user wants to
-- see. Returns an Exception with the bad string if the user suppled a bad
-- color name, or Success if the color is good.
color :: Parser (S.Runtime -> Ex.Exceptional String Chk.Colors)
color = parseOpt ["color"] "" (C.OneArg f)
  where
    f a1 rt = Ex.fromMaybe a1 (processColorArg rt a1)

processBackgroundArg ::
  String
  -> Maybe (Col.DrCrColors, Col.BaseColors)
processBackgroundArg x
  | x == "light" = return (LB.drCrColors, LB.baseColors)
  | x == "dark" = return (DB.drCrColors, DB.baseColors)
  | otherwise = Nothing


-- | Process an argument for the user's background color
-- choice. Returns an Exception with the bad string if the user
-- supplied a bad background name; Success otherwise.
background ::
  Parser (Ex.Exceptional String (Col.DrCrColors, Col.BaseColors))
background = parseOpt ["background"] "" (C.OneArg f)
  where
    f a1 = Ex.fromMaybe a1 (processBackgroundArg a1)

zeroBalances :: Parser CO.ShowZeroBalances
zeroBalances = showZb <|> hideZb
  where
    showZb = parseOpt ["show-zero-balances"] ""
             (C.NoArg (CO.ShowZeroBalances True))
    hideZb = parseOpt ["hide-zero-balances"] ""
             (C.NoArg (CO.ShowZeroBalances False))
