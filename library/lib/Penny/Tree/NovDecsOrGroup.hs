module Penny.Tree.NovDecsOrGroup where

import qualified Penny.Core.Anna as NovDecs
import qualified Penny.Core.Anna.DecDecsMayGroups as DecDecsMayGroups
import qualified Penny.Tree.NovDecsOrZero as NovDecsOrZero

-- | Parse tree for a value that begins either with a novem
-- or a grouping character.

data T r
  = LeadNovem NovDecs.T (Maybe (r, (DecDecsMayGroups.T r)))
  | LeadGroup r (NovDecsOrZero.T r)
  deriving (Eq, Ord, Show)
