module Penny.Lincoln.Ent.Error where

import qualified Penny.Lincoln.Ent.Code as EntCode
import qualified Penny.Lincoln.Trio as Trio
import qualified Penny.Imbalances as Imbalances

-- | An error occurred while attempting to create an 'Ent'.
data T = T
  { code :: EntCode.T
  -- ^ The exact nature of the error.

  , errTrio :: Trio.T
  -- ^ The 'T.Trio' that caused the error.

  , errBalances :: Imbalances.T
  -- ^ The balances that existed at the time the error occurred.
  } deriving (Eq, Ord, Show)

