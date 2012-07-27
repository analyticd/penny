module Penny.Cabin.Allocate (
  Allocation,
  allocation,
  unAllocation,
  allocate) where

import qualified Control.Monad.Trans.State as St
import qualified Data.Foldable as F
import qualified Data.Traversable as T
import qualified Data.Map as M

newtype Allocation = Allocation { unAllocation :: Int }
                     deriving (Show, Eq, Ord)

allocation :: Int -> Allocation
allocation i =
  if i < 1
  then error "Allocations must be at least 1"
  else Allocation i

-- | Divide up a whole number proportionally into several parts. The
-- sum of the parts is guaranteed to add up to original number.
allocate ::
  Ord k
  => M.Map k Allocation
  -- ^ Maps arbitrary unique values to the allocations, so that you
  -- can look up the corresponding allocations.

  -> Int
  -- ^ Allocated values will add up to this number.
  
  -> M.Map k Int
  -- ^ The given number, allocated into proportional parts.

allocate m t = let
  tot = F.sum . fmap (toDouble . unAllocation) $ m
  ratios = fmap ((/tot) . toDouble . unAllocation) m
  rounded = fmap (round . (* (toDouble t))) ratios
  toDouble = fromIntegral :: Int -> Double
  
  in if M.null m
     then M.empty
     else adjust rounded t

adjust ::
  Ord k
  => M.Map k Int
  -> Int
  -> M.Map k Int
adjust ws w = let
  wsInts = fmap fromIntegral ws
  diff = (fromIntegral w) - F.sum wsInts in
  if M.null ws
  then M.empty
  else if diff == 0
       then ws
       else let
         ws' = St.evalState (T.mapM adjustMap ws) diff
         in adjust ws' w

-- | The state is the target number minus the current actual total.
adjustMap :: Int -> St.State Int Int
adjustMap w = do
  diff <- St.get
  case compare diff 0 of
    EQ -> return w
    GT -> do
      St.put (pred diff)
      return (succ w)
    LT -> do
      St.put (succ diff)
      return (pred w)
