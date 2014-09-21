module Penny.Tree.LeadRadix where

import qualified Penny.Core.Anna.NovDecs as NovDecs
import qualified Penny.Core.Anna.DecDecsMayGroups as DecDecsMayGroups
import qualified Penny.Core.Anna.Zeroes as Zeroes
import qualified Penny.Tree.NovDecsOrGroup as NovDecsOrGroup

-- | Parse tree that leads with a radix point.
-- The radix point has already been parsed.

data T r
  = LeadZero Zeroes.T (Maybe (NovDecsOrGroup.T r))
  | LeadNovem NovDecs.T (Maybe (r, DecDecsMayGroups.T r))
  deriving (Eq, Ord, Show)

