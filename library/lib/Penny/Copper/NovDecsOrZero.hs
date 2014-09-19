module Penny.Copper.NovDecsOrZero where

import qualified Penny.Zeroes as Zeroes
import qualified Penny.Anna.NovSeqDecs as NovSeqDecs

-- | Parse tree for a value that begins with a novem or a zero.
data T r
  = LeadNovem (NovSeqDecs.T r)
  | LeadZero Zeroes.T
      (Maybe (Either (NovSeqDecs.T r) (r, T r)))
  deriving (Eq, Ord, Show)
