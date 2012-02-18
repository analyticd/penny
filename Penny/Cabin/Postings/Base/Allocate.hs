module Penny.Cabin.Allocate (
  Allocation,
  unAllocation,
  allocation,
  allocate) where

import qualified Control.Monad.Trans.State as St
import qualified Data.Foldable as F
import qualified Data.Traversable as T
import qualified Data.Map as M
import Data.Word (Word)

newtype Allocation = Allocation { unAllocation :: Word }
                     deriving (Show, Eq, Ord)

allocation :: Word -> Allocation
allocation i =
  if i < 1
  then error "Allocations must be at least 1"
  else Allocation i

allocate ::
  Ord k
  => M.Map k Allocation
  -> Word
  -> M.Map k Word
allocate m t = let
  tot = F.sum . fmap (fromIntegral . unAllocation) $ m
  ratios = fmap ((/tot) . fromIntegral . unAllocation) m
  rounded = fmap (round . (* (fromIntegral t))) ratios
  in if M.null m
     then M.empty
     else adjust rounded t

adjust ::
  Ord k
  => M.Map k Word
  -> Word
  -> M.Map k Word
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
adjustMap :: Word -> St.State Int Word
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
