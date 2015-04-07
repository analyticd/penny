module Penny.Lincoln.PluMin where

import Penny.Display

data PluMin = Plus | Minus
  deriving (Eq, Ord, Show)

instance Display PluMin where
  display Plus = ('+':)
  display Minus = ('-':)

class Signed a where
  sign :: a -> PluMin
  fromSign :: PluMin -> a

instance Signed PluMin where
  sign = id
  fromSign = id

