module Penny.NonZero
  ( NonZero
  , c'Integer'NonZero
  , c'NonZero'Integer
  , c'NonZero'Positive
  , c'Positive'NonZero
  , negate
  , pole
  , nonZeroSign
  ) where

import Penny.NonZero.Internal
import Penny.Positive.Internal
import Prelude hiding (negate)
import qualified Prelude
import Penny.Polar
import qualified Control.Lens as Lens

pole :: Lens.Lens' NonZero Pole
pole = Lens.lens get set
  where
    get (NonZero i)
      | i < 0 = negative
      | otherwise = positive
    set (NonZero i) new
      | i < 0 && new == positive = NonZero (Prelude.negate i)
      | i > 0 && new == negative = NonZero (Prelude.negate i)
      | otherwise = NonZero i

c'NonZero'Positive :: Positive -> NonZero
c'NonZero'Positive (Positive x) = NonZero x

c'Positive'NonZero :: NonZero -> Positive
c'Positive'NonZero (NonZero x)
  | x < 0 = Positive (Prelude.negate x)
  | otherwise = Positive x

negate :: NonZero -> NonZero
negate (NonZero x) = NonZero $ Prelude.negate x

nonZeroSign :: NonZero -> Pole
nonZeroSign (NonZero x)
  | x < 0 = negative
  | otherwise = positive
