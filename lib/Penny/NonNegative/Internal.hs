module Penny.NonNegative.Internal where


newtype NonNegative = NonNegative { c'Integer'NonNegative :: Integer }
  deriving (Eq, Ord, Show)

