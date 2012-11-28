module Penny.Cabin.Posts.Allocate (
  Allocation,
  allocation,
  unAllocation,
  allocate) where

import qualified Control.Monad.Trans.State as St
import qualified Data.Foldable as F
import qualified Data.Traversable as T

-- | Allocations are integers. The absolute value of the integer is
-- used, so for practical purposes @-4@ is the same allocation as @4@.
newtype Allocation = Allocation { unAllocation :: Int }
                     deriving (Show, Eq, Ord)

allocation :: Int -> Allocation
allocation = Allocation

-- | Properties of the allocated result:
--
-- * If the sum of the absolute values of the allocations is zero,
-- then each of the elements in the result will also be zero.
--
-- * Otherwise, if any allocation is zero, then the corresponding
-- amount in the result will also be zero. The sum of the results will
-- equal the total requested.
allocate ::
  (Functor f, F.Foldable f, T.Traversable f)
  => f Allocation
  -> Int
  -> f Int
allocate m t = let
  tot = F.sum . fmap (toDouble . abs . unAllocation) $ m
  ratios = fmap ((/tot) . toDouble . abs . unAllocation) m
  rounded = fmap (round . (* (toDouble t))) ratios
  toDouble = fromIntegral :: Int -> Double
  in if tot == 0
     then fmap (const 0) m
     else adjust rounded t

adjust :: (Functor f, F.Foldable f, T.Traversable f)
  => f Int
  -> Int
  -> f Int
adjust ws w = let
  wsInts = fmap fromIntegral ws
  diff = (fromIntegral w) - F.sum wsInts in
  if diff == 0
  then ws
  else let
    ws' = St.evalState (T.mapM adjustMap ws) diff
    in adjust ws' w

-- | The state is the target number minus the current actual total.
adjustMap :: Int -> St.State Int Int
adjustMap w = if w == 0 then return 0 else do
  diff <- St.get
  case compare diff 0 of
    EQ -> return w
    GT -> do
      St.put (pred diff)
      return (succ w)
    LT -> do
      St.put (succ diff)
      return (pred w)
