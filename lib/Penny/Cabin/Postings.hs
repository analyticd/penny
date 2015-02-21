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

-- | Represents a 'Qty' as \"smartly\" as possible, based on how its
-- corresponding 'Commodity' has been represented in the past.
--
-- The so-called \"mimode\" of a list @ls@ is the value appearing most
-- frequently in @ls@, if there is one.  If there is more than one
-- value that appears most frequently, one is chosen arbitrarily.  A
-- list has no mimode only if the list is empty.  For instance, the
-- mimode of @[1,2,3]@ may be any of 1, 2, or 3; the mimode of
-- @[1,1,2,3]@ is @1@; and @[]@ has no mimode.
--
-- In @repQtySmartly ei mp cy qt@, @mp@ is a map of 'Commodity' to
-- consider for history analysis.  Each 'Commodity' is paired with a
-- history list.  Each element of the history list is an 'Either',
-- with a 'Left' indicating that the radix was a comma, and a
-- 'Right' indicating the radix was a period.  The sequence contains
-- each grouping character used.
--
-- If the commodity @cy@ is found in the map @mp@, then the radix
-- point used is always the mimode radix point appearing in the
-- history list.  For that radix point, if there is a mimode grouping
-- character, then grouping is attempted using that grouping
-- character.  If there is no mimode grouping character, no grouping
-- is attempted.
--
-- If the commodity @cy@ is not found in the map @mp@, then the radix
-- point used is the mimode radix point for /all/ commodities in the
-- map @mp@.  For that radix point, if there is a mimode grouping
-- character, then grouping is attempted using that grouping
-- character.  If there is no mimode grouping character, no grouping
-- is attempted.
--
-- If the map @mp@ is completely empty, then the 'Qty' is rendered
-- using @ei@, where @ei@ is @Left Nothing@ for comma radix, no
-- grouping; @Right Nothing@ for period radix, no grouping; @Left
-- (Just c)@ for comma radix with attempted grouping using @c@; or
-- @Right (Just c)@ for period radix with grouping attempted using
-- @c@.
repQtySmartly
  :: Either (Maybe RadCom) (Maybe RadPer)
  -- ^ Default rendering
  -> M.Map Commodity (NonEmpty (Either (Seq RadCom) (Seq RadPer)))
  -- ^ History map
  -> Commodity
  -- ^ Associated commodity
  -> Qty
  -- ^ Render this 'Qty'
  -> QtyRepAnyRadix
repQtySmartly dflt mp cy = case repQtyByPopularCommodity mp cy of
  Just f -> f
  Nothing -> case map snd . M.assocs $ mp of
    [] -> repQty dflt
    x:xs -> repQtyByPopularity (F.foldl' (<>) x xs)

-- | Returns a function representing a Qty based on the radix point
-- and grouping character most frequently seen.
repQtyByPopularity
  :: NonEmpty (Either (Seq RadCom) (Seq RadPer))
  -- ^ History list
  -> Qty
  -> QtyRepAnyRadix
repQtyByPopularity = repQty . pickRadix


-- | If possible, returns a function representing a Qty based on the
-- representations that have been seen so far.  @historyRepresent m c@
-- is applied to a map, @m@, which holds all commodities that have
-- been seen with a quantity representation in their respective
-- 'Trio'.  The values in the map are, at minimum, the radix point,
-- and may also contain any grouping characters used.
-- 'historyRepresent' will return a function that renders 'Qty' for
-- that 'Commodity', but only if that 'Commodity' is a key in @m@.
repQtyByPopularCommodity
  :: M.Map Commodity (NonEmpty (Either (Seq RadCom) (Seq RadPer)))
  -- ^ History map
  -> Commodity
  -> Maybe (Qty -> QtyRepAnyRadix)
repQtyByPopularCommodity mp cy = fmap (repQty . pickRadix) (M.lookup cy mp)

-- | Picks the most popular radix point and, if possible, the most
-- popular grouping character corresponding to that radix.
pickRadix
  :: NonEmpty (Either (Seq RadCom) (Seq RadPer))
  -- ^ History list
  -> Either (Maybe RadCom) (Maybe RadPer)
pickRadix ne =
  let NonEmpty rdx _ = modeFromNonEmpty
        . fmap (either (const (Left Radix)) (const (Right Radix)))
        $ ne
  in case rdx of
      Left _ -> Left grpr
        where
          grpr = case avgMode . mconcat . lefts . seqFromNonEmpty $ ne of
            [] -> Nothing
            x:_ -> Just x
      Right _ -> Right grpr
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
