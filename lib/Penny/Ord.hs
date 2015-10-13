{-# LANGUAGE RankNTypes #-}
module Penny.Ord where

import Penny.Clatch
import Control.Lens (Getter, view)

comparing
  :: Ord a
  => Getter (Prefilt ()) a
  -> Prefilt ()
  -> Prefilt ()
  -> Ordering
comparing getter pa pb
  = compare (view getter pa) (view getter pb)
