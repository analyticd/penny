module Penny.Lincoln.Display where

import qualified Data.Sequence as S
import qualified Data.Foldable as F

-- | Display of representations.
class Display a where
  display :: a -> ShowS
  -- ^ Displays the value in a manner intended for end-user
  -- consumption.

instance Display Int where
  display = shows

instance Display Integer where
  display = shows

instance Display a => Display [a] where
  display = foldr (.) id . map display

instance Display a => Display (S.Seq a) where
  display = F.foldr (.) id . fmap display

instance (Display a, Display b) => Display (a, b) where
  display (a, b) = display a . display b

instance (Display a, Display b, Display c) => Display (a, b, c) where
  display (a, b, c) = display a . display b . display c

instance Display a => Display (Maybe a) where
  display Nothing = id
  display (Just x) = display x

