module Penny.Groups.AtLeast1 where

data AtLeast1 a = AtLeast1 { first :: a
                           , rest :: [a] }
                  deriving Show


