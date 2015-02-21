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

-- | Builds a map of all commodities and their corresponding radix
-- points and grouping characters.
renderingMap
  :: (Ledger l, Monad l, F.Foldable f)
  => f (Clatch l)
  -> l (M.Map Commodity (NonEmpty (Either (Seq RadCom) (Seq RadPer))))
renderingMap = F.foldlM f M.empty
  where
    f mp (Clatch _ _ pstg _) = do
      tri <- postingTrio pstg
      return $ case trioRendering tri of
        Nothing -> mp
        Just (cy, ei) -> case M.lookup cy mp of
          Nothing -> M.insert cy (NonEmpty ei S.empty) mp
          Just (NonEmpty o1 os) -> M.insert cy (NonEmpty o1 (os |> ei)) mp

-- | How is this Trio rendered?
trioRendering
  :: Trio
  -> Maybe (Commodity, (Either (Seq RadCom) (Seq RadPer)))
trioRendering tri = case tri of
  QC (QtyRepAnyRadix qr) cy _ -> Just (cy, ei)
    where
      ei = either (Left . mayGroupers) (Right . mayGroupers) qr
  UC (RepNonNeutralNoSide ei) cy _ ->
    Just (cy, either (Left . mayGroupers) (Right . mayGroupers) ei)
  _ -> Nothing


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
