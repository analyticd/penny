module Penny.Lincoln.PluMin where

data PluMin = Plus | Minus
  deriving (Eq, Ord, Show)

class Signed a where
  sign :: a -> PluMin

instance Signed PluMin where
  sign = id
