{-# LANGUAGE RankNTypes #-}
module Penny.Ord where

{-
  ( comparing
  , Down
  , down
  ) where

import Penny.Clatch
import Control.Lens (Getter, view, Profunctor, Contravariant, Optic', to)
import Data.Ord (Down(Down))

-- | Use to sort based on a 'Getter'.
--
-- >>> penny $ open "myfile" <> register <> sort (comparing date)
comparing
  :: Ord a
  => Getter (Prefilt ()) a
  -> Prefilt ()
  -> Prefilt ()
  -> Ordering
comparing getter pa pb
  = compare (view getter pa) (view getter pb)

-- | Use to reverse sort order.
--
-- >>> penny $ open "myfile" <> register <> sort (comparing (date . down))
down :: (Profunctor p, Contravariant f) => Optic' p f a (Down a)
down = to Down
-}
