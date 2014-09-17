module Penny.LeadRadix where

import qualified Penny.NovDecs as NovDecs
import qualified Penny.DecDecsMayGroups as DecDecsMayGroups
import qualified Penny.Zeroes as Zeroes
import qualified Penny.NovDecsOrGroup as NovDecsOrGroup

-- | Parse tree that leads with a radix point.
-- The radix point has already been parsed.

data T r
  = LeadZero Zeroes.T (Maybe (NovDecsOrGroup.T r))
  | LeadNovem NovDecs.T (Maybe (r, DecDecsMayGroups.T r))
  deriving (Eq, Ord, Show)

