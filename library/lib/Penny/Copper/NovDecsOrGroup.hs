module Penny.Copper.NovDecsOrGroup where

import qualified Penny.Lincoln.Anna as NovDecs
import qualified Penny.Lincoln.Anna.DecDecsMayGroups as DecDecsMayGroups
import qualified Penny.Copper.NovDecsOrZero as NovDecsOrZero

-- | Parse tree for a value that begins either with a novem
-- or a grouping character.

data T r
  = LeadNovem NovDecs.T (Maybe (r, (DecDecsMayGroups.T r)))
  | LeadGroup r (NovDecsOrZero.T r)
  deriving (Eq, Ord, Show)
