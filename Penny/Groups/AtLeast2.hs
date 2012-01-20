module Penny.Groups.AtLeast2 where

data AtLeast2 a = AtLeast2 { first :: a
                           , second :: a
                           , rest :: [a] }
