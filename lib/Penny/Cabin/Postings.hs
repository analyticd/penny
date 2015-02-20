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
import Data.Monoid

-- | Examines all available Trio to obtain a function that will
-- render a given Qty given its corresponding Commodity.
--
-- Examines every Trio.  If the Trio has both a quantity
-- representation and a commodity, takes note of which radix point was
-- used and whether the digits are grouped.
--
-- The function returned will represent Qty.  For any given Commodity,
-- it uses the radix point and grouping character that is seen most
-- frequently for the given Commodity.  The function will fail if the
-- given Commodity has not been seen.
howToRender
  :: (Ledger l, Monad l, T.Traversable t1)
  => t1 (Clatch l)
  -> l (Commodity -> Maybe (Qty -> QtyRepAnyRadix))
howToRender = undefined

-- | Determines how to render, given a non-empty list of items.
pickRenderer
  :: NonEmpty (Either (Seq RadCom) (Seq RadPer))
  -> Qty
  -> QtyRepAnyRadix
pickRenderer ls q = case pickRadix ls of
  Left (rdxCom, Nothing) -> QtyRepAnyRadix . Left . QtyRep
    . NilOrBrimPolar $ case repUngroupedQty rdxCom q of
        Center c -> Center (NilU c)
        OffCenter o p -> OffCenter (BrimUngrouped o) p
  Right (rdxPer, Nothing) -> QtyRepAnyRadix . Right . QtyRep
    . NilOrBrimPolar $ case repUngroupedQty rdxPer q of
        Center c -> Center (NilU c)
        OffCenter o p -> OffCenter (BrimUngrouped o) p

pickRadix
  :: NonEmpty (Either (Seq RadCom) (Seq RadPer))
  -> Either (Radix RadCom, Maybe RadCom) (Radix RadPer, Maybe RadPer)
pickRadix ne =
  let NonEmpty rdx _ = modeFromNonEmpty
        . fmap (either (const (Left Radix)) (const (Right Radix)))
        $ ne
  in case rdx of
      Left radC -> Left (radC, grpr)
        where
          grpr = case avgMode . mconcat . lefts . seqFromNonEmpty $ ne of
            [] -> Nothing
            x:_ -> Just x
      Right radP -> Right (radP, grpr)
        where
          grpr = case avgMode . mconcat . rights . seqFromNonEmpty $ ne of
            [] -> Nothing
            x:_ -> Just x

lefts :: F.Foldable f => f (Either a b) -> [a]
lefts = F.foldr f []
  where
    f ei acc = case ei of
      Left l -> l : acc
      Right _ -> acc

rights :: F.Foldable f => f (Either a b) -> [b]
rights = F.foldr f []
  where
    f ei acc = case ei of
      Left _ -> acc
      Right r -> r : acc


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


avgMode
  :: (Ord a, F.Foldable f)
  => f a
  -> [a]
avgMode = finish . M.toList . F.foldl' fldr M.empty
  where
    fldr mp val = case M.lookup val mp of
      Nothing -> M.insert val 1 mp
      Just old -> M.insert val (succ old) mp
    finish pairs = map fst . filter isMaxPair $ pairs
      where
        isMaxPair (_, v) = v == maxVal
          where
            maxVal = maximum . ((0 :: Integer) :) . map snd $ pairs

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
