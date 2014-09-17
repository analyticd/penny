module Penny.NovDecsOrGroup where

import qualified Penny.NovDecs as NovDecs
import qualified Penny.DecDecsMayGroups as DecDecsMayGroups
import qualified Penny.NovDecsOrZero as NovDecsOrZero

-- | Parse tree for a value that begins either with a novem
-- or a grouping character.

data T r
  = LeadNovem NovDecs.T (Maybe (r, (DecDecsMayGroups.T r)))
  | LeadGroup r (NovDecsOrZero.T r)
  deriving (Eq, Ord, Show)
