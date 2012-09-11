-- | Command line parsers that are common to various Cabin reports.

module Penny.Cabin.Parsers where

import Control.Applicative ((<|>))
import qualified Penny.Cabin.Colors as Col
import qualified Penny.Cabin.Colors.DarkBackground as DB
import qualified Penny.Cabin.Colors.LightBackground as LB
import qualified Penny.Cabin.Options as CO
import qualified System.Console.MultiArg.Combinator as C
import System.Console.MultiArg.Prim (Parser)

parseOpt :: [String] -> [Char] -> C.ArgSpec a -> Parser a
parseOpt ss cs a = C.parseOption [C.OptSpec ss cs a]


-- | Process an argument for how many colors the user wants to
-- see. Returns an Exception with the bad string if the user suppled a bad
-- color name, or Success if the color is good.
color :: Parser CO.ColorPref
color = parseOpt ["color"] "" (C.ChoiceArg ls)
  where
    ls = [("yes", CO.Pref8), ("no", CO.Pref0),
          ("auto", CO.PrefAuto), ("256", CO.Pref256)]


-- | Process an argument for the user's background color
-- choice. Returns an Exception with the bad string if the user
-- supplied a bad background name; Success otherwise.
background :: Parser (Col.DrCrColors, Col.BaseColors)
background = parseOpt ["background"] "" (C.ChoiceArg ls)
  where
    ls = [ ("light", (LB.drCrColors, LB.baseColors))
         , ("dark", (DB.drCrColors, DB.baseColors)) ]


zeroBalances :: Parser CO.ShowZeroBalances
zeroBalances = showZb <|> hideZb
  where
    showZb = parseOpt ["show-zero-balances"] ""
             (C.NoArg (CO.ShowZeroBalances True))
    hideZb = parseOpt ["hide-zero-balances"] ""
             (C.NoArg (CO.ShowZeroBalances False))

ascending :: Parser ()
ascending = parseOpt ["ascending"] "" (C.NoArg ())

descending :: Parser ()
descending = parseOpt ["descending"] "" (C.NoArg ())
