module Penny.Lincoln.Groups.Family.Child where

data Child p c =
  Child { this :: c
        , sibling1 :: c
        , siblings :: [c]
        , parent :: p }
  deriving Show
