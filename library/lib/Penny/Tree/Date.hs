module Penny.Tree.Date where

import qualified Penny.Tree.Digits.D4 as D4
import qualified Penny.Tree.Digits.D1or2 as D2
import qualified Penny.Tree.Date.Sep as Sep

import qualified Penny.Tree.Parsec as P

data T = T D4.T Sep.T D2.T Sep.T D2.T
  deriving (Eq, Ord, Show)

parser :: P.Parser T
parser = P.liftM5 T D4.parser Sep.parser D2.parser Sep.parser D2.parser
