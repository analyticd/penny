module Penny.Harvest.Collected.Error where

import qualified Penny.Harvest.Collected.Error.Inline as Error.Inline
import qualified Penny.Harvest.Collected.Error.Final as Error.Final
import qualified Penny.Harvest.Locate.Located as Located
import Data.Sequence (Seq)

-- | All errors, both inline and final, that may occur during
-- collection.
data T = T
  { inlines :: Seq (Located.T Error.Inline.T)
  , final :: Maybe Error.Final.T
  } deriving (Eq, Ord, Show)
