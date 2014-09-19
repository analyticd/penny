module Penny.Copper.LeadRadix where

import qualified Penny.Lincoln.Anna as NovDecs
import qualified Penny.Lincoln.Anna.DecDecsMayGroups as DecDecsMayGroups
import qualified Penny.Lincoln.Anna.Zeroes as Zeroes
import qualified Penny.Copper.NovDecsOrGroup as NovDecsOrGroup

-- | Parse tree that leads with a radix point.
-- The radix point has already been parsed.

data T r
  = LeadZero Zeroes.T (Maybe (NovDecsOrGroup.T r))
  | LeadNovem NovDecs.T (Maybe (r, DecDecsMayGroups.T r))
  deriving (Eq, Ord, Show)

