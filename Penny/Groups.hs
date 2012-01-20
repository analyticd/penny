module Penny.Groups where

import Penny.Groups.AtLeast1.Data
import qualified Penny.Groups.AtLeast1.Data as A1

data Sibling a = Sibling { this :: a,
                           others :: AtLeast1 a }
