module Penny.Lincoln.Family.Child where

data Child p c =
  Child { child :: c
        , sibling1 :: c
        , siblings :: [c]
        , parent :: p }
  deriving Show
