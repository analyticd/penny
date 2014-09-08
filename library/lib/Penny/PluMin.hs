module Penny.PluMin where

import Deka.Dec

data T
  = Plus
  | Minus
  deriving (Eq, Ord, Show)

toSign :: T -> Sign
toSign Plus = Sign0
toSign Minus = Sign1

fromSign :: Sign -> T
fromSign Sign0 = Plus
fromSign Sign1 = Minus
