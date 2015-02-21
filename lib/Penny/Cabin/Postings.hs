-- | The Postings report
module Penny.Cabin.Postings where

import Control.Applicative
import Penny.Lincoln
import Rainbox
import qualified Data.Traversable as T
import qualified Data.Foldable as F
import qualified Data.Map.Strict as M
import Data.Sequence (Seq, (|>), ViewL(..), (<|))
import qualified Data.Sequence as S
import Control.Monad
import Data.Semigroup

newtype VisibleSer = VisibleSer Serset
  deriving (Eq, Ord, Show)

data Tranche l = Tranche (Clatch l) FilteredSer VisibleSer Balances

postingsBox
  :: (Applicative l, T.Traversable t1, T.Traversable t2)
  => t1 (Tranche l -> l Cell)
  -> t2 (Tranche l)
  -> l Box
postingsBox cols
  = fmap (gridByRows . F.toList . fmap F.toList)
  . makeRows cols

makeRows
  :: (Applicative l, T.Traversable t1, T.Traversable t2)
  => t1 (Tranche l -> l Cell)
  -> t2 (Tranche l)
  -> l (t2 (t1 Cell))
makeRows cols = T.sequenceA . fmap T.sequenceA . fmap mkRow
  where
    mkRow trch = fmap ($ trch) cols
