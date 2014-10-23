-- after Timothy Thomas Fortune

module Penny.Core.Fortune where

import Data.Sequence (Seq)
import qualified Data.Sequence as Seq

import qualified Penny.Core.Bundle as Bundle
import qualified Penny.Core.Serial as Serial

-- | A 'Penny.Core.Bundle.T', combined with an additional serial
-- indicating the order in which this posting appears after filtering.
-- Also contains balance information.

data T = T
  { bundle :: Bundle.T
  , postMainFilter :: Serial.T
  -- ^ After postings are filtered to determine which postings will be
  -- part of a report, they are assigned this serial.
  , balance ::
  } deriving (Eq, Ord, Show)

filterBundles
  :: (Bundle.T -> Bool)
  -> Seq Bundle.T
  -> Seq T
filterBundles p sq
  = makeItems
  . Seq.filter p
  $ sq
  where
    makeItems results = Seq.zipWith T results
      . Serial.serials . Seq.length $ results
