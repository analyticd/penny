module Penny.Groups.AtLeast1 where

import Penny.Groups.AtLeast1.Data
import Penny.Groups

siblings :: AtLeast1 a -> AtLeast1 (Sibling a)
siblings a = AtLeast1 f r where
  f = Sibling (first a

