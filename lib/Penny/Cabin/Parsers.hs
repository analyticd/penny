-- | Command line parsers that are common to various Cabin reports.

module Penny.Cabin.Parsers where

import qualified Penny.Cabin.Options as CO
import qualified System.Console.MultiArg.Combinator as C


zeroBalances :: C.OptSpec CO.ShowZeroBalances
zeroBalances = C.OptSpec ["zero-balances"] "" (C.ChoiceArg ls)
  where
    ls = [ ("show", CO.ShowZeroBalances True)
         , ("hide", CO.ShowZeroBalances False) ]

data SortOrder = Ascending | Descending

order :: C.OptSpec SortOrder
order = C.OptSpec ["order"] "" (C.ChoiceArg ls)
  where
    ls = [ ("ascending", Ascending)
         , ("descending", Descending) ]

help :: C.OptSpec ()
help = C.OptSpec ["help"] "h" (C.NoArg ())
