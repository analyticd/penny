module Penny.Lincoln.Family.Family where

data Family p c =
  Family { parent :: p
         , child1 :: c
         , child2 :: c
         , children :: [c] }
  deriving (Eq, Show)
