module Penny.Cabin.Postings.Combinator.Allocate (allocate) where

import qualified Control.Monad.Trans.State as St
import qualified Data.Foldable as F
import Data.List.NonEmpty (NonEmpty)
import qualified Data.Traversable as T
import Data.Word (Word)

-- | Allocate an integer evenly between the number of fractions
-- given. The allocation is not guaranteed to be exactly proportional,
-- but it is guaranteed to add up to the original integer given.
allocate :: NonEmpty Double -> Word -> NonEmpty Word
allocate ds w = let
  tot = F.sum ds
  ratios = fmap (/ tot) ds
  rounded = fmap (round . (* (fromIntegral w))) ratios
  in adjust rounded w

adjust :: NonEmpty Word -> Word -> NonEmpty Word
adjust ws w = let
  wsInts = fmap fromIntegral ws
  diff = (fromIntegral w) - F.sum wsInts in
  if diff == 0
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
