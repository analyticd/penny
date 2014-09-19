-- | An Ingot where the radix point is a period.
--
-- Parsing a 'T' will succeed if there is a "Penny.Tree.Currency" to
-- parse, even where there is no "Penny.Tree.Lewis" to parse.  This is
-- in contrast to "Penny.Tree.Ingot.Comma", which will fail if there
-- is a "Penny.Tree.Currency" on the left but no "Penny.Tree.Lewis";
-- the idea is that if the user enters the apostrophe, he must also
-- intend to enter a "Penny.Tree.Lewis".

module Penny.Tree.Ingot.Period where

import qualified Penny.Tree.Lewis as Lewis
import qualified Penny.Core.Anna.RadPer as RadPer
import qualified Penny.Tree.Currency as Currency

data T
  = Currency Currency.T (Maybe (Lewis.T RadPer.T))
  | Lewis (Lewis.T RadPer.T) (Maybe Currency.T)
  deriving (Eq, Ord, Show)
