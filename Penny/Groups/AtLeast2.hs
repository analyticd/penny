module Penny.Groups.AtLeast2 where

import qualified Penny.Groups.AtLeast1 as A1

data AtLeast2 a = AtLeast2 { first :: a
                           , second :: a
                           , rest :: [a] }
