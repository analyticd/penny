module Penny.Core.Ent.Error where

import qualified Penny.Core.Ent.Code as EntCode
import qualified Penny.Core.Trio as Trio
import qualified Penny.Core.Imbalances as Imbalances

-- | An error occurred while attempting to create an 'Ent'.
data T = T
  { code :: EntCode.T
  -- ^ The exact nature of the error.

  , errTrio :: Trio.T
  -- ^ The 'T.Trio' that caused the error.

  , errBalances :: Imbalances.T
  -- ^ The balances that existed at the time the error occurred.
  } deriving (Eq, Ord, Show)

