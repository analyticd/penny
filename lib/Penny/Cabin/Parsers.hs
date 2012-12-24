-- | Command line parsers that are common to various Cabin reports.

module Penny.Cabin.Parsers where

import qualified Penny.Cabin.Colors as Col
import qualified Penny.Cabin.Colors.DarkBackground as DB
import qualified Penny.Cabin.Colors.LightBackground as LB
import qualified Penny.Cabin.Options as CO
import qualified System.Console.MultiArg.Combinator as C


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
