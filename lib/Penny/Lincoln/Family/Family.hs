module Penny.Lincoln.Family.Family where

-- | A Family has one parent (ah, the anomie, sorry) and at least two
-- children.
data Family p c =
  Family { parent :: p
         , child1 :: c
         , child2 :: c
         , children :: [c] }
  deriving (Eq, Show)
