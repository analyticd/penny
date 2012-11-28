module Penny.Lincoln.Family.Child where

-- | A Child has at least one sibling and a parent.
data Child p c =
  Child { child :: c
        , sibling1 :: c
        , siblings :: [c]
        , parent :: p }
  deriving Show
