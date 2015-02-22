-- | The Postings report
module Penny.Cabin.Postings where

import Control.Applicative
import Penny.Lincoln
import Rainbox
import qualified Data.Traversable as T
import qualified Data.Foldable as F
import Data.Text (Text)
import qualified Data.Text as X

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

data CellTag
  = DebitTag
  | CreditTag
  | ZeroTag
  | InfoTag
  | NoticeTag
  deriving (Eq, Ord, Show, Enum, Bounded)

type SimpleCell l = Tranche l -> l (CellTag, Text)

spacer :: Applicative l => Int -> SimpleCell l
spacer i _ = pure (InfoTag, X.replicate i (X.singleton ' '))
