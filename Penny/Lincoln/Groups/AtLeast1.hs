module Penny.Lincoln.Groups.AtLeast1 where

data AtLeast1 a = AtLeast1 { first :: a
                           , rest :: [a] }
                  deriving (Eq, Ord, Show)


