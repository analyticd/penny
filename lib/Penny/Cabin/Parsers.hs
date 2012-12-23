-- | Command line parsers that are common to various Cabin reports.

module Penny.Cabin.Parsers where

import qualified Penny.Cabin.Colors as Col
import qualified Penny.Cabin.Colors.DarkBackground as DB
import qualified Penny.Cabin.Colors.LightBackground as LB
import qualified Penny.Cabin.Options as CO
import qualified Penny.Shield as S
import qualified Penny.Cabin.Chunk as Chk
import qualified System.Console.MultiArg.Combinator as C


-- | Process an argument for how many colors the user wants to
-- see. Returns an Exception with the bad string if the user suppled a
-- bad color name, or Success if the color is good.
color :: C.OptSpec (S.Runtime -> Chk.Colors)
color = C.OptSpec ["color"] "" (C.ChoiceArg ls)
  where
    ls = [ ("yes", const Chk.Colors8)
         , ("no", const Chk.Colors0)
         , ("auto", Chk.maxCapableColors)
         , ("256", const Chk.Colors256)
         ]


-- | Process an argument for the user's background color
-- choice. Returns an Exception with the bad string if the user
-- supplied a bad background name; Success otherwise.
background :: C.OptSpec (Col.DrCrColors, Col.BaseColors)
background = C.OptSpec ["background"] "" (C.ChoiceArg ls)
  where
    ls = [ ("light", (LB.drCrColors, LB.baseColors))
         , ("dark", (DB.drCrColors, DB.baseColors)) ]


zeroBalances :: [C.OptSpec CO.ShowZeroBalances]
zeroBalances = [showZb, hideZb]
  where
    showZb = C.OptSpec ["show-zero-balances"] ""
             (C.NoArg (CO.ShowZeroBalances True))
    hideZb = C.OptSpec ["hide-zero-balances"] ""
             (C.NoArg (CO.ShowZeroBalances False))

ascending :: C.OptSpec ()
ascending = C.OptSpec ["ascending"] "" (C.NoArg ())

descending :: C.OptSpec ()
descending = C.OptSpec ["descending"] "" (C.NoArg ())
