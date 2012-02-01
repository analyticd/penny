module Penny.Family.Family where

data Family p c =
  Family { parent :: p
         , child1 :: c
         , child2 :: c
         , children :: [c] }
  deriving Show
