module Penny.Tree.Digits.D4 where

import qualified Penny.Tree.Digit as D
import Penny.Tree.Parsec

data T = T D.T D.T D.T D.T
  deriving (Eq, Ord, Show)

parser :: Parser T
parser = liftM4 T p p p p
  where
    p = D.parser

toInt :: T -> Int
toInt (T d3 d2 d1 d0)
  = D.toInt d3 * 1000
  + D.toInt d2 * 100
  + D.toInt d1 * 10
  + D.toInt d0
