module Penny.Groups.AtLeast1.Data where

data AtLeast1 a = AtLeast1 { first :: a
                           , rest :: [a] }


