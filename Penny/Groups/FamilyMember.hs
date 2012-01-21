module Penny.Groups.FamilyMember where

import Penny.Groups.AtLeast1

data FamilyMember a = FamilyMember { member :: a
                                   , siblings :: AtLeast1 a }
                      deriving Show
