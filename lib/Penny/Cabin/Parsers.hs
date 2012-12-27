-- | Command line parsers that are common to various Cabin reports.

module Penny.Cabin.Parsers where

import qualified Penny.Cabin.Options as CO
import qualified System.Console.MultiArg.Combinator as C


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
